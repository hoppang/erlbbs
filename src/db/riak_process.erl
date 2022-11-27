-module(riak_process).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
% 인터페이스
-export([add_user/2, get_all_users_id/0, get_all_posts_title/0, new_post/2]).

-record(state, {pid}).

start_link(Addr, Port) ->
    ?LOG_DEBUG("riak_process start_link ~p ~p", [Addr, Port]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Addr, Port], []).

init([Addr, Port]) ->
    ?LOG_NOTICE("riak_process init ~p ~p", [Addr, Port]),

    case riakc_pb_socket:start_link(Addr, Port) of
        {ok, Pid} ->
            ?LOG_NOTICE("Successfully connected to DB."),
            {ok, #state{pid = Pid}};
        {error, {tcp, econnrefused}} ->
            ?LOG_ERROR("Failed to init Riak DB: connection refused"),
            ignore;
        Err ->
            ?LOG_ERROR("Failed to init Riak DB: unknown error ~p", [Err]),
            ignore
    end.

%% ================================================================================
%% 인터페이스 함수

-spec add_user(binary(), binary()) -> ok.
add_user(Id, Pw) ->
    gen_server:cast(riak_process, {new, <<"Users">>, Id, Pw}),
    ok.

-spec get_all_users_id() -> [binary()].
get_all_users_id() ->
    gen_server:call(riak_process, {read_all, <<"Users">>}).

-spec new_post(binary(), binary()) -> ok.
new_post(Content, Title) ->
    gen_server:cast(riak_process, {new, <<"Posts">>, Title, Content}),
    ok.

-spec get_all_posts_title() -> [binary()].
get_all_posts_title() ->
    gen_server:call(riak_process, {read_all, <<"Posts">>}).

%% ================================================================================

%% 버킷에서 항목 하나 읽기
handle_call({read, Bucket, Key}, _From, State) ->
    ?LOG_DEBUG("read obj", []),
    {state, Pid} = State,

    % 항목 읽기
    case riakc_pb_socket:get(Pid, Bucket, Key) of
        {ok, Result} ->
            [Val] = riakc_obj:get_values(Result),
            ?LOG_DEBUG("riakc get_values = ~p", [Val]),
            {reply, Val, State};
        Err ->
            ?LOG_ERROR("No entry: ~p", [Err]),
            {reply, "", State}
    end;
%% 버킷에 해당하는 항목 전부 읽기
handle_call({read_all, Bucket}, _From, State) ->
    ?LOG_DEBUG("read all keys from bucket ~p", [Bucket]),
    {state, Pid} = State,

    case riakc_pb_socket:list_keys(Pid, Bucket) of
        {ok, Result} ->
            ?LOG_DEBUG("read_all result: ~p~n", [Result]),
            {reply, Result, State};
        Err ->
            ?LOG_ERROR("No keys: ~p", [Err]),
            {reply, nil, State}
    end.

%% 버킷에 새 키/값 추가
handle_cast({new, Bucket, Key, Value}, State) ->
    ?LOG_DEBUG("new obj"),
    {state, Pid} = State,
    NewObj = riakc_obj:new(Bucket, Key, Value),
    riakc_pb_socket:put(Pid, NewObj),
    {noreply, State};
%% 버킷에 있는 키의 값 업데이트
handle_cast({update, Bucket, Key, NewValue}, State) ->
    % 항목 업데이트
    ?LOG_DEBUG("update obj ~p ~p ~p", [Bucket, Key, NewValue]),
    {state, Pid} = State,
    {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
    UpdatedObj = riakc_obj:update_value(Obj, NewValue),
    ?LOG_DEBUG("Updated obj = ~p", [UpdatedObj]),
    ok = riakc_pb_socket:put(Pid, UpdatedObj),
    {noreply, State};
%% 처리되지 않은 메시지
handle_cast(AnyMsg, State) ->
    ?LOG_DEBUG("handle_cast ~p", [AnyMsg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?LOG_DEBUG("handle_info", []),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_DEBUG("terminate", []),
    ok.
