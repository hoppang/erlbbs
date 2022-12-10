-module(index_controller).

-export([init/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), list()}.
init(Req0, State) ->
    ?LOG_INFO("INDEX INIT ~p", State),

    % 유저 ID 목록 출력(그냥 보여주기용)
    UserIds =
        lists:map(fun({user, _Id, Username, _Password}) -> Username end, db:select_all_users()),
    % 글 제목 목록 출력(동일)
    Articles = db:select_all_articles_metadata(),

    {ok, Body} = index_view:render([{users, UserIds}, {articles, Articles}]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req0),

    {ok, Req1, State}.

-spec terminate(atom(), cowboy_req:req(), list()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
