-module(atem_cli).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
      ip,
      socket,
      program=0,
      preview=0
     }).

start_link(IP) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [IP], []).

init([IP]) ->
  self() ! connect,
    {ok, #state{ip=IP}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, #state{ip=IP}=State) ->
  {ok, Socket} = gen_tcp:connect(IP,4242,[binary,{active,once},{packet, line}]),
  {noreply, State#state{socket=Socket}};

handle_info({tcp,Socket,<<"READY\n">>}, #state{socket=Socket}=State) ->
  gen_tcp:send(Socket, <<"status\n">>),
  inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp,Socket,Data}, #state{socket=Socket}=State) ->
  [D0,D1] = binary:split(Data, <<"\n">>),
  Cmd=parse_atem_command(D0),
  %io:format("Got command ~s ~p~n",[Data,Cmd]),
  S1=case Cmd of
       {preview, N} ->
         switched(preview, State#state.preview, N, State#state.program),
         State#state{preview=N};
       {program, N} ->
         switched(program, State#state.program, State#state.preview, N),
         State#state{program=N};
       {fader_pos, N} when N>0 andalso N<10000 ->
         command_camera(State#state.preview, program),
         State;
       _ ->
         State
     end,
  case D1 of
    <<>> ->
      inet:setopts(Socket, [{active, once}]),
      {noreply, S1};
    _ ->
      handle_info({tcp, Socket, D1}, S1)
  end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_atem_command(<<"PREV: ",N/binary>>) ->
  {preview, binary_to_integer(N)};

parse_atem_command(<<"PROG: ",N/binary>>) ->
  {program, binary_to_integer(N)};

parse_atem_command(<<"TPOS: ",N/binary>>) ->
  {fader_pos, binary_to_integer(N)};

parse_atem_command(<<"DSK: ",N:1/binary,": ",State/binary>>) ->
  {{dsk,binary_to_integer(N)}, case State of <<"true">> -> true; _ -> false end};

parse_atem_command(Any) ->
  {unknown, Any}.

switched(Mode, From, Prev, Prog) ->
  All=lists:usort([From,Prev,Prog]),
  lists:foreach(
    fun(Camera) ->
        if(Camera == Prog) ->
          command_camera(Camera, program);
        (Camera == Prev) ->
          command_camera(Camera, preview);
        true ->
          command_camera(Camera, off)
        end
    end, All),
  io:format("~s from ~w to ~w + ~w~n",[Mode,From,Prev,Prog]).

command_camera(ID, Mode) ->
  case global:whereis_name({camera, ID}) of
    undefined -> ok;
    PID when is_pid(PID) ->
      gen_server:cast(PID,{camera,ID,Mode})
  end.

