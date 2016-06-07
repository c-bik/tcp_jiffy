-module(srv_port).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

-define(L(__F,__A), io:format("[~p:~p] "__F"~n", [?MODULE, ?LINE | __A])).
-define(L(__F),     ?L(__F, [])).

start_link(Ip, Port) when is_integer(Port) ->
    {ok, Addr} = inet:getaddr(Ip, inet),
    IpStr = inet:ntoa(Addr),
    gen_server:start_link(?MODULE, [IpStr, Port], []).

%% Callbacks
init([Ip, Port]) ->
    process_flag(trap_exit, true),
    PrivDir = case code:priv_dir(tcp_jiffy) of
        {error,_} -> "./priv/";
        PDir -> PDir
    end,
    case os:find_executable("srv_port", PrivDir) of
        false ->
            case os:find_executable("srv_port", "./deps/tcp_jiffy/priv/") of
                false -> {stop, bad_executable};
                Executable ->
                    start_exe(Executable, Ip, Port)
            end;
        Executable ->
            start_exe(Executable, Ip, Port)
    end.

start_exe(Executable, Ip, Port) when is_integer(Port) ->
    start_exe(Executable, Ip, integer_to_list(Port));
start_exe(Executable, Ip, Port) ->
    case (catch open_port(
                {spawn_executable, Executable},
                [{packet, 4}, binary, exit_status, use_stdio,
                 {args, [Ip, Port]}])) of
        {'EXIT', Reason} ->
            ?L("open port failed ~p", [Reason]),
            {stop, Reason};
        PortHandle ->
            ?L("Port ~p", [erlang:port_info(PortHandle)]),
            {ok, #state{port=PortHandle}}
    end.

handle_call(Req, From, State) ->
    ?L("unexpected call ~p ~p", [From, Req]),
    {stop, unimplemented, unimplemented, State}.

handle_cast(Msg, State) ->
    ?L("unexpected cast ~p", [Msg]),
    {stop, unimplemented, State}.

-define(CONNECT,    0).
-define(DISCONNECT, 1).
-define(DATA,       2).
-define(LOG,        3).
handle_info({Port, {data, <<?DATA:8,Sock:32,Data/binary>>}}, #state{port=Port} = State) ->
    ?L("~p: Data ~p", [Sock, Data]),
    {noreply, State};
handle_info({Port, {data, <<?DISCONNECT:8,_:24,Sock:32>>}}, #state{port=Port} = State) ->
    ?L("disconnect ~p", [Sock]),
    {noreply, State};
handle_info({Port, {data, <<?CONNECT:8,Ip1:8,Ip2:8,Ip3:8,Ip4:8,Prt:16,Sock:32>>}},
            #state{port=Port} = State) ->
    ?L("Connect ~s:~p ~p", [inet:ntoa({Ip4,Ip3,Ip2,Ip1}), Prt, Sock]),
    {noreply, State};
handle_info({Port, {data, <<?LOG:8,Log/binary>>}}, #state{port=Port} = State) ->
    ?L("~s", [Log]),
    {noreply, State};
handle_info({Port, {data, <<>>}}, #state{port=Port} = State) ->
    {noreply, State};
handle_info({Port, {data, Data}}, #state{port=Port} = State) ->
    ?L("RX ~w", [Data]),
    {noreply, State};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    ?L("~p exit with status ~p", [Port, Status]),
    {stop, port_exit, State};
handle_info(Info, State) ->
    ?L("unexpected info ~p", [Info]),
    {stop, unimplemented, State}.

terminate(Reason, #state{port=Port}) ->
    ?L("exit port with reason ~p", [Reason]),
    case erlang:port_info(Port) of
        undefined -> ?L("port already dead");
        _ ->
            case catch port_close(Port) of
                true -> ok;
                Error ->
                    ?L("error closing port: ~p", [Error])
            end
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
