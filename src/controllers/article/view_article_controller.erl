-module(view_article_controller).

%% protocols
-export([init/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), list()) -> {ok | cowboy_rest, cowboy_req:req(), list()}.
init(Req, State) ->
    ?LOG_INFO("VIEW ARTICLE INIT ~p", [Req]),

    {ok, Body} = view_article_view:render([]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),

    {ok, Req1, State}.

-spec terminate(atom(), cowboy_req:req(), list()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
