-module(index_controller).

-export([init/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), list()}.
init(Req0, State) ->
    ?LOG_INFO("INDEX INIT ~p", State),
    {ok, Body} = index_view:render([]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req0),
    {ok, Req1, State}.

-spec terminate(atom(), cowboy_req:req(), list()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
