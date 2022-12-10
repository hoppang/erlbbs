-module(view_article_controller).

%% protocols
-export([init/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), list()) -> {ok | cowboy_rest, cowboy_req:req(), list()}.
init(Req, State) ->
    ?LOG_INFO("VIEW ARTICLE INIT ~p", [Req]),

    #{id := Id} = cowboy_req:match_qs([id], Req),
    IdNum = util:bitstring_to_integer(Id),

    {article, _Id, Title, Content} = db:read_article(IdNum),

    {ok, Body} = view_article_view:render([{id, IdNum}, {title, Title}, {content, Content}]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),

    {ok, Req1, State}.

-spec terminate(atom(), cowboy_req:req(), list()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
