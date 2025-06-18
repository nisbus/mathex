%% Mathex MCP Server
-module(mathex_mcp_server).

-export([start/0, start/1, stop/0]).

%% simple line based protocol: each request is an Erlang term ending with '.'
%% Example: {sum,[1,2,3]}.

-define(DEFAULT_PORT, 4040).

start() ->
    start(?DEFAULT_PORT).

start(Port) when is_integer(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, line},
                                         {active, false}, {reuseaddr, true}]),
    spawn(fun() -> accept(Listen) end),
    {ok, Listen}.

stop() ->
    ok.

accept(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            spawn(fun() -> accept(Listen) end),
            loop(Socket);
        {error, _} ->
            ok
    end.

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Response = handle_request(Line),
            gen_tcp:send(Socket, Response ++ "\n"),
            loop(Socket);
        {error, closed} ->
            ok
    end.

handle_request(Line) ->
    Str = binary_to_list(Line),
    case parse_term(Str) of
        {ok, {Func, Args}} ->
            Result = (catch apply(mathex, Func, Args)),
            io_lib:format("~p", [Result]);
        {error, Reason} ->
            io_lib:format("error: ~p", [Reason])
    end.

parse_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> {ok, Term};
                Error -> {error, Error}
            end;
        Error -> {error, Error}
    end.

