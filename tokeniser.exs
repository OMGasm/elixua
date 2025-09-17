defmodule Token do
  defstruct [:type, :val, :line, :pos]

  def new(type, val \\ nil, line, pos)

  def new(type, val, line, pos) do
    %Token{type: type, val: val, line: line, pos: pos}
  end
end

defmodule Ctx do
  @enforce_keys [:str, :line, :pos, :stream]
  defstruct [:str, :line, :pos, :stream]

  def next(ctx, chars \\ 1)

  def next(%Ctx{} = ctx, chars) when is_integer(chars) do
    str = String.slice(ctx.str, chars..-1//1)
    %{ctx | str: str, pos: ctx.pos + chars}
  end

  def next(%Ctx{} = ctx, chars) when is_binary(chars) do
    next(ctx, String.length(chars))
  end

  def next(%Ctx{} = ctx, :line) do
    case Enum.take(ctx.stream, 1) do
      [str] -> %{ctx | str: str, line: ctx.line + 1, pos: 1}
      [] -> %{ctx | str: ""}
    end
  end

  def new(stream) do
    [str] = Enum.take(stream, 1)
    %Ctx{str: str, line: 1, pos: 1, stream: stream}
  end
end

defmodule Tokeniser do
  @moduledoc """
  fuck
  """

  @doc """
  foo
  """
  def tokenise(stream) do
    stream
    |> Ctx.new()
    |> token_iterate()
  end

  def reconstruct(tokens) do
    tokens
    |> Enum.map(&recon/1)
    |> Enum.map(&IO.write/1)
  end

  def recon(%Token{type: type, val: val} = _token) do
    case type do
      :EOL -> "\n"
      :space -> val
      :ident -> val
      :kw -> val
      :token -> val
      :num -> val
      :num_hex -> "0x#{val}"
      :shebang -> "#!#{val}"
      :string_sq -> "'#{val}'"
      :string_dq -> ~s("#{val}")
      :comment -> "--#{val}"
      :comment_dsq -> "--[[#{val}]]"
    end
  end

  def token_iterate(ctx) do
    case(tok(ctx)) do
      {:ok, %Token{type: :EOL} = token, _} -> [token] ++ token_iterate(Ctx.next(ctx, :line))
      {:ok, %Token{} = token, ctx} -> [token] ++ token_iterate(ctx)
      {:EOF, _, _} -> []
    end
  end

  @alpha_cap ?A..?Z
  @alpha_low ?a..?z
  @num ?0..?9
  @hex Enum.concat([@num, ?a..?f, ?A..?F])
  @ident_start Enum.concat([@alpha_low, @alpha_cap, [?_]])
  @ident_mid Enum.concat(@ident_start, @num)
  @spaces [?\s, ?\t]
  @tok_2 ["==", "~=", "<=", ">=", ".."]
  @tok_1 ~c(+-*/%^#<>=(\){}[];:,.;)
  @kw_6 ["elseif", "return", "repeat"]
  @kw_5 ["false", "local", "while", "until", "break"]
  @kw_4 ["true", "then", "else"]
  @kw_3 ["and", "not", "nil", "for", "end"]
  @kw_2 ["or", "in", "do", "if"]
  @quote [?', ?"]

  def tok_ok(type, val, ctx, consume \\ nil) do
    {
      :ok,
      Token.new(type, val, ctx.line, ctx.pos),
      Ctx.next(ctx, consume || val)
    }
  end

  # first-line shebang
  def tok(%Ctx{str: <<"#!", shebang::binary>>, line: 1, pos: 1} = ctx) do
    len = String.length(shebang)
    shebang = String.slice(shebang, 0, len - 1)
    tok_ok(:shebang, shebang, ctx, len + 1)
  end

  # keywords
  def tok(%Ctx{str: <<"function", s, _::binary>>} = ctx)
      when s not in @ident_mid do
    tok_ok(:kw, "function", ctx)
  end

  def tok(%Ctx{str: <<kw::binary-size(6), s, _::binary>>} = ctx)
      when kw in @kw_6 and s not in @ident_mid do
    tok_ok(:kw, kw, ctx)
  end

  def tok(%Ctx{str: <<kw::binary-size(5), s, _::binary>>} = ctx)
      when kw in @kw_5 and s not in @ident_mid do
    tok_ok(:kw, kw, ctx)
  end

  def tok(%Ctx{str: <<kw::binary-size(4), s, _::binary>>} = ctx)
      when kw in @kw_4 and s not in @ident_mid do
    tok_ok(:kw, kw, ctx)
  end

  def tok(%Ctx{str: <<kw::binary-size(3), s, _::binary>>} = ctx)
      when kw in @kw_3 and s not in @ident_mid do
    tok_ok(:kw, kw, ctx)
  end

  def tok(%Ctx{str: <<kw::binary-size(2), s, _::binary>>} = ctx)
      when kw in @kw_2 and s not in @ident_mid do
    tok_ok(:kw, kw, ctx)
  end

  # end of keywords

  # start of ident
  def tok(%Ctx{str: <<c, _::binary>>} = ctx)
      when c in @ident_start do
    {:ok, name} = ident(Ctx.next(ctx), <<c>>)
    tok_ok(:ident, name, ctx)
  end

  # numbers

  # hex
  def tok(%Ctx{str: <<"0x", _::binary>>} = ctx) do
    ctx = Ctx.next(ctx, 2)
    {:ok, num} = number(ctx, "", :hex)
    tok_ok(:num_hex, num, ctx)
  end

  # integer
  def tok(%Ctx{str: <<c, _::binary>>} = ctx)
      when c in @num do
    {:ok, num} = number(Ctx.next(ctx), <<c>>, :int)
    tok_ok(:num, num, ctx)
  end

  # comments
  def tok(%Ctx{str: <<"--[[", _::binary>>} = ctx) do
    res =
      ctx
      |> Ctx.next(4)
      |> delimited("", "]]", :multiline)

    case res do
      {:ok, comment, ctx2} ->
        {
          :ok,
          Token.new(:comment_dsq, comment, ctx.line, ctx.pos),
          ctx2
        }

      {:notfound, partial, ctx} ->
        {:err, :unclosed, partial, ctx}
    end
  end

  def tok(%Ctx{str: <<"--", rest::binary>>} = ctx) do
    len = String.length(rest)
    comment = String.slice(rest, 0, len - 1)
    tok_ok(:comment, comment, ctx, len + 1)
  end

  # tokens
  def tok(%Ctx{str: <<"...", _::binary>>} = ctx) do
    tok_ok(:token, "...", ctx)
  end

  def tok(%Ctx{str: <<t::binary-size(2), _::binary>>} = ctx)
      when t in @tok_2 do
    tok_ok(:token, t, ctx)
  end

  def tok(%Ctx{str: <<t, _::binary>>} = ctx)
      when t in @tok_1 do
    tok_ok(:token, <<t>>, ctx)
  end

  # end of tokens

  # empty string literal
  def tok(%Ctx{str: <<q, q, _::binary>>} = ctx)
      when q in @quote do
    tok_ok(str_literal_type(q), "", ctx, 2)
  end

  # string literal
  def tok(%Ctx{str: <<q, _::binary>>} = ctx)
      when q in @quote do
    {:ok, str, _} =
      ctx
      |> Ctx.next()
      |> delimited("", q, :singleline)

    tok_ok(str_literal_type(q), str, ctx, String.length(str) + 2)
  end

  # empty multiline string literal
  def tok(%Ctx{str: <<"[[]]">>} = ctx) do
    tok_ok(:string_multiline, "", ctx, 4)
  end

  # multiline string literal
  def tok(%Ctx{str: <<"[[", _::binary>>, line: line, pos: pos} = ctx) do
    {:ok, str, ctx} =
      ctx
      |> Ctx.next(2)
      |> delimited("", "]]", :multiline)

    {
      :ok,
      Token.new(:string_multiline, str, line, pos),
      ctx
    }
  end

  # EOL
  def tok(%Ctx{str: "\n"} = ctx) do
    tok_ok(:EOL, "\n", ctx)
  end

  # EOF
  def tok(%Ctx{str: ""} = ctx) do
    {:EOF, nil, ctx}
  end

  def tok(%Ctx{str: <<sp, _::binary>>} = ctx)
      when sp in @spaces do
    tok_ok(:space, <<sp>>, ctx)
  end

  # middle of ident
  def ident(%Ctx{str: <<c, _::binary>>} = ctx, cur)
      when c in @ident_mid do
    ident(Ctx.next(ctx), cur <> <<c>>)
  end

  # end of ident
  def ident(%Ctx{}, cur) do
    {:ok, cur}
  end

  # hex number
  def number(%Ctx{str: <<c, _::binary>>} = ctx, num, :hex)
      when c in @hex do
    number(Ctx.next(ctx), num <> <<c>>, :hex)
  end

  # integer
  def number(%Ctx{str: <<c, _::binary>>} = ctx, num, :int)
      when c in @num do
    number(Ctx.next(ctx), num <> <<c>>, :hex)
  end

  # end hex/int number
  def number(%Ctx{}, num, radix)
      when radix in [:hex, :int, :dec] do
    {:ok, num}
  end

  def str_literal(%Ctx{str: <<c, q, _::binary>>} = ctx, str, q)
      when c != ?\\ do
    {:ok, str <> <<c>>, Ctx.next(ctx, 2)}
  end

  def str_literal(%Ctx{str: <<c::binary-size(1), _::binary>>} = ctx, str, q) do
    str_literal(Ctx.next(ctx), str <> c, q)
  end

  def str_literal_type(q) do
    case q do
      ?' -> :string_sq
      ?" -> :string_dq
    end
  end

  def delimited(ctx, body, delim, mode)

  def delimited(%Ctx{str: <<delim::binary-size(2), _::binary>>} = ctx, body, delim, _) do
    {:ok, body, Ctx.next(ctx, 2)}
  end

  def delimited(%Ctx{str: <<delim::binary>>} = ctx, body, delim, _) do
    {:ok, body, Ctx.next(ctx, delim)}
  end

  def delimited(%Ctx{str: <<delim, _::binary>>} = ctx, body, delim, _) do
    {:ok, body, Ctx.next(ctx)}
  end

  def delimited(%Ctx{str: "\n"} = ctx, body, delim, :multiline) do
    ctx
    |> Ctx.next(:line)
    |> delimited(body <> "\n", delim, :multiline)
  end

  def delimited(%Ctx{str: <<c, _::binary>>} = ctx, body, delim, mode) do
    delimited(Ctx.next(ctx), body <> <<c>>, delim, mode)
  end

  def delimited(%Ctx{str: "\n"} = ctx, body, _, :singleline) do
    {:notfound, body, ctx}
  end

  def delimited(%Ctx{str: ""} = ctx, body, _, _) do
    {:notfound, body, ctx}
  end

  def delimited_escaped(ctx, body, delim, mode) do
    res = delimited(ctx, body, delim, mode)

    case res do
      {:ok, partial, ctx2} ->
        case String.slice(partial, -1, 1) do
          "\\" -> delimited_escaped(ctx2, body <> partial <> delim, delim, mode)
          _ -> res
        end

      _ ->
        res
    end
  end

  #
end

System.argv()
|> Enum.map(&IO.inspect/1)
|> Enum.map(fn x -> File.open!(x) |> IO.stream(:line) end)
|> Enum.map(&Tokeniser.tokenise/1)
|> Enum.map(&Tokeniser.reconstruct/1)
