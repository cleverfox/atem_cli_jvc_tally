%%%-------------------------------------------------------------------
%% @doc atemjvc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(atemjvc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok,Config}=file:consult("app.config"),
  io:format("Config ~p~n",[Config]),
  AtemCli={atemcli,
           {atem_cli,start_link,[proplists:get_value(atem_cli,Config,"127.0.0.1")]},
           permanent,
           2000,
           worker,
           []
          },
  Cameras=lists:map(
            fun({ID, IP, Pw}) ->
                {{camera,ID},
                 {jvc_camera,start_link,[ID, IP, Pw]},
                 permanent,
                 2000,
                 worker,
                 []
                }
            end, proplists:get_value(cameras, Config, [])
           ),
  {ok, { {one_for_all, 0, 1}, [
                              AtemCli
                              ]++Cameras
       } }.

%%====================================================================
%% Internal functions
%%====================================================================
