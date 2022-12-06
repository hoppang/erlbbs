%% @doc DB 작업자 프로세스. mnesia 사용
-module(db).

-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init/0, add_user/2, add_article/2, select_all_articles/0, select_all_users/0,
         select_user/1]).

-record(user, {id, username, password}).
-record(article, {id, title, content}).

init() ->
    ?LOG_NOTICE("Start mnesia db process"),
    mnesia:create_schema([node()]),
    mnesia:start(),

    create_table(users, user, record_info(fields, user)),
    create_table(articles, article, record_info(fields, article)),

    mnesia:wait_for_tables([users, articles], 10000),
    ok.

-spec add_user(bitstring(), bitstring()) -> {atomic, ok}.
add_user(Username, Password) ->
    Fun = fun() ->
             mnesia:write(users,
                          #user{id = erlang:unique_integer(),
                                username = Username,
                                password = Password},
                          write)
          end,
    {atomic, ok} = mnesia:transaction(Fun).

-spec add_article(bitstring(), bitstring()) -> {atomic, ok}.
add_article(Title, Content) ->
    Fun = fun() ->
             mnesia:write(articles,
                          #article{id = erlang:unique_integer(),
                                   title = Title,
                                   content = Content},
                          write)
          end,
    {atomic, ok} = mnesia:transaction(Fun).

%% @doc 모든 유저 정보를 select
-spec select_all_users() -> [{user, integer(), bitstring(), bitstring()}].
select_all_users() ->
    do_query(qlc:q([X || X <- mnesia:table(users)])).

%% @doc 주어진 이름을 가진 유저를 select
-spec select_user(integer() | bitstring()) ->
                     [{user, integer(), bitstring(), bitstring()}].
% ID
select_user(Id) when is_integer(Id) ->
    ?LOG_DEBUG("Select user by ID: ~p", [Id]),
    do_query(qlc:q([X || X <- mnesia:table(users), X#user.id == Id]));
% 유저 이름(bitstring)
select_user(Username) ->
    ?LOG_DEBUG("Select user by username: ~p", [Username]),
    do_query(qlc:q([X || X <- mnesia:table(users), X#user.username == Username])).

-spec select_all_articles() -> [{article, integer(), bitstring(), bitstring()}].
select_all_articles() ->
    do_query(qlc:q([X || X <- mnesia:table(articles)])).

% ================================================================================

-spec create_table(atom(), atom(), [atom()]) -> ok.
create_table(TableName, RecordName, RecordInfo) ->
    case mnesia:create_table(TableName,
                             [{record_name, RecordName},
                              {attributes, RecordInfo},
                              {disc_copies, [node()]}])
    of
        {atomic, ok} ->
            ?LOG_NOTICE("Create table ~p (record_name: ~p) OK", [TableName, RecordName]);
        {aborted, {already_exists, _}} ->
            ?LOG_NOTICE("Create table ~p OK (record_name: ~p, already exists)",
                        [TableName, RecordName]);
        Err ->
            ?LOG_ERROR("Failed to create mnesia table ~p: ~p", [TableName, Err]),
            throw("Failed to create mnesia table")
    end,
    ok.

-spec do_query(qlc:query_handle()) -> any().
do_query(Query) ->
    Fun = fun() -> qlc:e(Query) end,
    {atomic, Val} = mnesia:transaction(Fun),
    Val.
