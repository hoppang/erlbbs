-module(index_controller).

-export([init/2, terminate/3]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    {ok, Body} = index_view:render([]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req0),
    {ok, Req1, State}.

-spec terminate(any(), any(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
