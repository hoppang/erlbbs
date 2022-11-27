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
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/", index_controller, []},
                                 {"/register", register_controller, []},
                                 {"/new_post", new_post_controller, []}]}]),
    ListeningPort = 60000,

    {ok, DbAddr} = application:get_env(db, address),
    {ok, DbPort} = application:get_env(db, port),

    riak_process:start_link(DbAddr, DbPort),

    %% http server 설정
    {ok, _Pid} =
        cowboy:start_clear(http, [{port, ListeningPort}], #{env => #{dispatch => Dispatch}}),

    ?LOG_NOTICE("Init OK: Now you can access index page via http://127.0.0.1:~p",
                [ListeningPort]),

    erlbbs_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
