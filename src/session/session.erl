-module(session).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    ?LOG_NOTICE("Starting session process..."),
    gen_server:start_link({local, session}, session, [], []).

init(_Args) ->
    {ok, nil}.

handle_call(_, _From, State) ->
    {reply, nil, State}.

handle_cast(_, State) ->
    {noreply, State}.
