defmodule Token do
  defstruct [:type, :val, :line, :pos]

  def new(type, val \\ nil, line, pos)

  def new(type, val, line, pos) do
    %Token{type: type, val: val, line: line, pos: pos}
  end
end

defmodule Ctx do
  @enforce_keys [:str, :line, :pos]
  defstruct [:str, :line, :pos]

  def next(ctx, chars \\ 1)

  def next(%Ctx{} = ctx, chars) when is_integer(chars) do
    <<_::binary-size(chars), rest::binary>> = ctx.str
    %{ctx | str: rest, pos: ctx.pos + chars}
  end

  def next(%Ctx{} = ctx, chars) when is_binary(chars) do
    next(ctx, String.length(chars))
  end

  def new({str, line}) do
    %Ctx{str: str, line: line, pos: 1}
  end
end

defmodule Tokeniser do
  @moduledoc """
  fuck
  """

  @doc """
  foo
  """
  def tokenise(str) do
    Stream.with_index(str, 1)
    |> Stream.map(&token_iterate(Ctx.new(&1)))
  end

  def reconstruct(str) do
    Enum.map(str, &Function.identity/1)
    |> Enum.reduce(fn a, c -> c ++ a end)
    |> Enum.map(&dbg/1)
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
      :shebang -> val
      :string_sq -> "'#{val}'"
      :string_dq -> ~s("#{val}")
    end
  end

  def token_iterate(ctx) do
    case(tok(ctx)) do
      {:ok, %Token{type: :EOL} = token, _} -> [token]
      {:ok, %Token{} = token, ctx} -> [token] ++ token_iterate(ctx)
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
    tok_ok(:shebang, shebang, ctx, len)
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
    {:ok, str, ctx} = str_literal(Ctx.next(ctx), "", q)
    tok_ok(str_literal_type(q), str, ctx, 0)
  end

  # EOL
  def tok(%Ctx{str: "\n"} = ctx) do
    tok_ok(:EOL, "\n", ctx)
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

  #
end

System.argv()
|> Enum.map(&IO.inspect/1)
|> Enum.map(fn x -> File.open!(x) |> IO.stream(:line) end)
|> Enum.map(&Tokeniser.tokenise/1)
|> Enum.map(&Tokeniser.reconstruct/1)
|> Enum.map(&Stream.run/1)
