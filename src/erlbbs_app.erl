%%%-------------------------------------------------------------------
%% @doc erlbbs public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbbs_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    ?LOG_NOTICE("Start application"),

    %% cowboy router 설정
    Dispatch = cowboy_router:compile([{'_', [{"/", index_controller, []}]}]),
    ListeningPort = 60000,

    %% http server 설정
    {ok, _Pid} =
        cowboy:start_clear(http, [{port, ListeningPort}], #{env => #{dispatch => Dispatch}}),

    erlbbs_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
