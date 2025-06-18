FROM erlang:26
WORKDIR /usr/src/mathex
COPY . .
RUN rebar3 compile
EXPOSE 4040
CMD ["erl","-noshell","-pa","_build/default/lib/mathex/ebin","-eval","mathex_mcp_server:start(), timer:sleep(infinity)"]
