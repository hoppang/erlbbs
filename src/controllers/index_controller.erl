-module(index_controller).

-export([init/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-spec init(cowboy_req:req(), list()) -> {ok, cowboy_req:req(), list()}.
init(Req0, State) ->
    ?LOG_INFO("INDEX INIT ~p", State),

    % 유저 ID 목록 출력(그냥 보여주기용)
    Users = db:select_all_users(),
    ?LOG_NOTICE("USERS: ~p", [Users]),

    UsersWithDelim =
        lists:map(fun({user, _Id, Username, _Password}) -> <<Username/binary, <<" ">>/binary>>
                  end,
                  Users),
    {ok, Body} = index_view:render([{users, list_to_binary(UsersWithDelim)}]),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req0),

    {ok, Req1, State}.

-spec terminate(atom(), cowboy_req:req(), list()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
