%% Mathex MCP Client
-module(mathex_mcp_client).

-export([call/1, call/2, call/3]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 4040).

call(Term) ->
    call(?DEFAULT_HOST, ?DEFAULT_PORT, Term).

call(Port, Term) when is_integer(Port) ->
    call(?DEFAULT_HOST, Port, Term);
call(Host, Term) when is_list(Host); is_atom(Host) ->
    call(Host, ?DEFAULT_PORT, Term).

call(Host, Port, Term) ->
    Options = [binary, {packet, line}],
    case gen_tcp:connect(Host, Port, Options) of
        {ok, Socket} ->
            Request = io_lib:format("~p.~n", [Term]),
            gen_tcp:send(Socket, Request),
            case gen_tcp:recv(Socket, 0) of
                {ok, Line} ->
                    gen_tcp:close(Socket),
                    parse_response(binary_to_list(Line));
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, Reason}
            end;
        Error -> Error
    end.

parse_response(Str) ->
    Trim = string:trim(Str),
    case string:prefix(Trim, "error:") of
        true ->
            ErrStr = string:trim(string:substr(Trim, 7)),
            case parse_term(ErrStr) of
                {ok, Term} -> {error, Term};
                Other -> Other
            end;
        false ->
            parse_term(Trim)
    end.

parse_term(Text) ->
    case erl_scan:string(Text) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> {ok, Term};
                Error -> {error, Error}
            end;
        Error -> {error, Error}
    end.

