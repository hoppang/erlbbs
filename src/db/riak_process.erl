-module(riak_process).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {pid}).

start_link() ->
    ?LOG_DEBUG("riak_process start_link"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?LOG_DEBUG("riak_process init"),

    {ok, DbAddr} = application:get_env(db, address),
    {ok, DbPort} = application:get_env(db, port),

    case riakc_pb_socket:start_link(DbAddr, DbPort) of
        {ok, Pid} ->
            ?LOG_NOTICE("Successfully connected to DB."),
            {ok, #state{pid = Pid}};
        {error, {tcp, econnrefused}} ->
            ?LOG_ERROR("Failed to init Riak DB: connection refused"),
            %{stop, econnrefused};
            ignore;
        Err ->
            ?LOG_ERROR("Failed to init Riak DB: unknown error ~p", [Err]),
            %{stop, unknownerror}
            ignore
    end.

handle_call({read, Bucket, Key}, _From, State) ->
    ?LOG_NOTICE("read obj", []),
    {state, Pid} = State,

    % 항목 읽기
    case riakc_pb_socket:get(Pid, Bucket, Key) of
        {ok, Result} ->
            [Val] = riakc_obj:get_values(Result),
            ?LOG_NOTICE("riakc get_values = ~p", [Val]),
            {reply, Val, State};
        Err ->
            ?LOG_ERROR("No entry: ~p", [Err]),
            {reply, "", State}
    end.

handle_cast({new, Bucket, Key, Value}, State) ->
    ?LOG_NOTICE("new obj"),
    {state, Pid} = State,
    NewObj = riakc_obj:new(Bucket, Key, Value),
    riakc_pb_socket:put(Pid, NewObj),
    {noreply, State};
handle_cast({update, Bucket, Key, NewValue}, State) ->
    % 항목 업데이트
    ?LOG_NOTICE("update obj ~p ~p ~p", [Bucket, Key, NewValue]),
    {state, Pid} = State,
    {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
    UpdatedObj = riakc_obj:update_value(Obj, NewValue),
    ?LOG_DEBUG("Updated obj = ~p", [UpdatedObj]),
    ok = riakc_pb_socket:put(Pid, UpdatedObj),
    {noreply, State};
handle_cast(AnyMsg, State) ->
    ?LOG_DEBUG("handle_cast ~p", [AnyMsg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?LOG_DEBUG("handle_info", []),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_DEBUG("terminate", []),
    ok.
