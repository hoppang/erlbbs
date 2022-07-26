-module(new_article_controller).

%% protocols
-export([init/2, terminate/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
%% callback
-export([handle_post/2]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), list()) -> {ok | cowboy_rest, cowboy_req:req(), list()}.
init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {cowboy_rest, Req, State};
        <<"GET">> ->
            {ok, Body} = new_article_view:render([]),
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
            {ok, Req1, State}
    end.

-spec terminate(atom(), cowboy_req:req(), list()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

-spec allowed_methods(cowboy_req:req(), list()) -> {[binary()], cowboy_req:req(), list()}.
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), list()) ->
                                {[{binary(), atom()}], cowboy_req:req(), list()}.
content_types_accepted(Req, State) ->
    {[{<<"application/x-www-form-urlencoded">>, handle_post}], Req, State}.

-spec handle_post(cowboy_req:req(), list()) ->
                     {{boolean(), binary()}, cowboy_req:req(), list()}.
handle_post(Req, State) ->
    {ok, Body, _Req} = cowboy_req:read_urlencoded_body(Req),
    {_TitleKey, Title} = lists:keyfind(<<"title">>, 1, Body),
    {_ContentKey, Content} = lists:keyfind(<<"content">>, 1, Body),
    ?LOG_INFO("New article success? ~p ~p", [Title, Content]),
    db:add_article(Title, Content),
    % 작업 완료 후 /register 로 리디렉션
    {{true, <<"/">>}, Req, State}.
