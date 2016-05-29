-module(srv_port).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port}).

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
            io:format("open port failed ~p~n", [Reason]),
            {stop, Reason};
        PortHandle ->
            {ok, #state{port=PortHandle}}
    end.

handle_call(Req, From, State) ->
    io:format("unexpected call ~p ~p~n", [From, Req]),
    {stop, unimplemented, unimplemented, State}.

handle_cast(Msg, State) ->
    io:format("unexpected cast ~p~n", [Msg]),
    {stop, unimplemented, State}.

handle_info({Port, {data, <<>>}}, #state{port=Port} = State) ->
    {noreply, State};
handle_info({Port, {data, <<4,Log/binary>>}}, #state{port=Port} = State) ->
    io:format("~s", [Log]),
    {noreply, State};
handle_info({Port, {data, Data}}, #state{port=Port} = State) ->
    io:format("RX ~p~n", [Data]),
    {noreply, State};
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    io:format("~p exit with status ~p~n", [Port, Status]),
    {stop, port_exit, State};
handle_info(Info, State) ->
    io:format("unexpected info ~p~n", [Info]),
    {stop, unimplemented, State}.

terminate(Reason, #state{port=Port}) ->
    io:format("exit port with reason ~p", [Reason]),
    catch port_close(Port).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
