-module(jvc_camera).

-export([authenticate/2,
		api_request/3]).

-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record (state, {
		   id,
		   ip,
		   password,
		   token,
		   tmr,
		   mode
		  }).

start_link(ID, IP, Password) when is_integer(ID),
								  is_binary(Password),
								  is_list(IP)  ->
    gen_server:start_link({global, {camera, ID}}, ?MODULE, [ID, IP, Password], []).

init([ID, IP, Password]) ->
	%global:register_name({camera,ID},self()),
	self() ! update_token,
    {ok, #state{id=ID, ip=IP, password=Password}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({camera,N, Value}, State=#state{id=ID}) when ID==N ->
	io:format("cam ~w ~s~n",[N,Value]),
	catch erlang:cancel_timer(State#state.tmr),
	{noreply, State#state{mode = Value,
						  tmr = erlang:send_after(10, self(), update_tally)
						 }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_token, State=#state{ip=IP,password=PW}) ->
	{ok,SID}=authenticate(IP,PW),
	{noreply, State#state{token=SID}};

handle_info(update_tally, State) ->
	catch erlang:cancel_timer(State#state.tmr),
	api_request(State#state.ip,
				State#state.token,
				#{<<"Command">>=><<"SetStudioTally">>,
				  <<"Params">>=>
				  #{<<"Indication">>=>
					case State#state.mode of
						program -> <<"Program">>;
						preview -> <<"Preview">>;
						off -> <<"Off">>
					end }}),
	{noreply, State#state{tmr=undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_auth(Headers) ->
	<<"Digest ",Dgst/binary>>=list_to_binary(proplists:get_value("www-authenticate",Headers)),
	lists:foldl(
	  fun(Component,Acc) ->
			  [Key,RVal]=binary:split(Component,<<"=">>),
			  Val=case re:run(RVal,<<"^\"(.+)\"$">>,[{capture,all_but_first,binary}]) of
					  nomatch -> RVal;
					  {match, [V1]} -> V1
				  end,
			  maps:put(Key,Val,Acc)
	  end, #{},
	  binary:split(Dgst,<<", ">>,[global])
	 ).

make_auth(ServerHdrs, URI, Username, Password) ->
	Realm=maps:get(<<"realm">>, ServerHdrs),
	CNonce=base64:encode(crypto:strong_rand_bytes(6)),
	Nonce=maps:get(<<"nonce">>,ServerHdrs),
	NC= <<"00000001">>,
	HA1=hex:encode(crypto:hash(md5,<<Username/binary,":",Realm/binary,":",Password/binary>>)),
	HA2=hex:encode(crypto:hash(md5,<<"GET:",URI/binary>>)),
	RespStr= <<HA1/binary,":",Nonce/binary,":",NC/binary,":",CNonce/binary,":auth:",HA2/binary>>,
	Resp=hex:encode(crypto:hash(md5,RespStr)),
	DS=maps:fold(
	  fun(K,V,A) ->
			  Pre=case A of <<>> ->
								<<>>;
							_ -> <<A/binary,", ">>
				  end,
			  <<Pre/binary,K/binary,"=\"",V/binary,"\"">>
	  end,
	  <<>>,
	  ServerHdrs#{
		<<"username">> => Username,
		<<"uri">> => URI,
		<<"cnonce">> => CNonce,
		<<"nc">> => NC,
		<<"response">> => Resp
	   }),
	"Digest "++binary_to_list(DS).

%Digest username="jvc", realm="GY-HM200", nonce="e212f90d1b5d0c688fc835ba05af741e", uri="/api.php", cnonce="Y2JjMDQ5N2QwNzUxMGM3MzYxZDZmZmM5Y2ViYmI0YWI=", nc=00000001, qop=auth, response="9936cb1d0dd926ac4075a909650a1421"

get_sessionid([]) ->
	error;

get_sessionid([{"set-cookie","SessionID="++Cookie}|_]) ->
	{ok,list_to_binary(Cookie)};

get_sessionid([_|Rest]) ->
	get_sessionid(Rest).


authenticate(IP, Password) ->
	{ok, {
	   {"HTTP/1.1",401,_},
	   Headers,
	   _Body}} = httpc:request(get,
							   {"http://"++IP++"/api.php",
								[]}, [], [{body_format, binary}]),
	Digest=make_auth(parse_auth(Headers), <<"/api.php">>, <<"jvc">>, Password),
	{ok, {{"HTTP/1.1",200,_},
		  Headers2,
		  _Body2}} = httpc:request(get,
							   {"http://"++IP++"/api.php",
								[{"Authorization",Digest}]}, [], [{body_format, binary}]),
	get_sessionid(Headers2).

api_request(IP, SID, JSON) ->
	%curl -v 'http://192.168.252.144/cgi-bin/api.cgi' --data '{"Request":{"Command":"SetStudioTally","SessionID":"d320c7e1eafe56394530bb704310aa50","Params":{"Indication":"Preview"}}}'
	B=jsx:encode(#{
				   <<"Request">> => JSON#{
									  <<"SessionID">> => SID
									 }
	   }),
	io:format("JSON ~s~n",[B]),
	{ok, {{"HTTP/1.1",200,_},
		  Headers,
		  Body}} = httpc:request(post, 
								 {"http://"++IP++"/cgi-bin/api.cgi", [], "application/json", B}, 
								 [], [{body_format,binary}]),
	CT=proplists:get_value("content-type",Headers),
	%io:format("Body ~p~n",[Body]),
	case CT of
		"application/json" -> #{<<"Response">> := Re } = jsx:decode(Body, [return_maps]), Re;
		_ -> Body
	end.
	

