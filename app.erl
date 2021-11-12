-module(app).
-export([iniciar/0]).
-define(SERVIDOR, ?MODULE).
-define(SEPARADOR, "----------------------------------").
-define(CABECALHO, "Menu de opções dos Empreendimentos").
-define(OPCOES, [
  [{id, 1}, {nome, "Listar"}, {acao, fun listar/0}],
  [{id, 2}, {nome, "Adicionar"}, {acao, fun adicionar/0}],
  [{id, 3}, {nome, "Buscar"}, {acao, fun buscar/0}],
  [{id, 4}, {nome, "Editar"}, {acao, fun editar/0}],
  [{id, 5}, {nome, "Deletar"}, {acao, fun deletar/0}],
  [{id, 6}, {nome, "Sair"}, {acao, fun sair/0}]
]).
-define(PERGUNTA, "O que deseja fazer?").
-define(ERRO_ID_NAO_ENCONTRADO, {erro, id_nao_encontrado}).

iniciar() ->
  iniciar([]).

iniciar(Empreendimentos)
  when is_list(Empreendimentos) ->
    imprimir_menu(),
    Resposta = pegar_resposta_do_usuario(),
    Resultado = case pegar_acao_da_opcao_por_id(Resposta) of
      {ok, Fun} -> Fun();
      ?ERRO_ID_NAO_ENCONTRADO -> ?ERRO_ID_NAO_ENCONTRADO
    end,
    io:format("Resultado: ~p~n", [Resultado]),
    iniciar(Empreendimentos).

imprimir(Valor)
  when is_list(Valor) ->
    io:format("~s~n", [Valor]);

imprimir(Valor) ->
    io:format("~p~n", [Valor]).

imprimir_menu() ->
  imprimir(?SEPARADOR),
  imprimir(?CABECALHO),
  imprimir(?SEPARADOR),
  imprimir_opcoes(),
  imprimir(?SEPARADOR).

imprimir_opcoes() ->
  lists:foreach(fun imprimir/1, opcoes_com_id_e_nome_concatenados()).

opcoes_com_id_e_nome_concatenados() ->
  lists:map(fun concatenar_id_e_nome_de_opcao/1, ?OPCOES).

concatenar_id_e_nome_de_opcao([{id, Id}, {nome, Nome}, _Fun]) ->
  lists:concat([Id, ": ", Nome]).

pegar_resposta_do_usuario() ->
  Pergunta = io_lib:format("~s ", [?PERGUNTA]),
  Resposta = string:trim(io:get_line(Pergunta)),
  list_to_integer(Resposta).

pegar_acao_da_opcao_por_id(Id)
  when is_integer(Id) ->
    case tentar_econtrar_opcao_por_id(Id, ?OPCOES) of
      {value, [_Id, _Nome, {acao, Fun}]} -> {ok, Fun};
      false -> ?ERRO_ID_NAO_ENCONTRADO
    end.

tentar_econtrar_opcao_por_id(Id, Opcoes) ->
  lists:search(fun(Opcao) -> opcao_possui_o_id(Id, Opcao) end, Opcoes).

opcao_possui_o_id(Id, [{id, Id}, _Nome, _Fun]) -> true;
opcao_possui_o_id(_IdOpcao, [_Id, _Nome, _Fun]) -> false.

listar() -> {ok, listar}.
adicionar() -> {ok, adicionar}.
buscar() -> {ok, buscar}.
editar() -> {ok, editar}.
deletar() -> {ok, deletar}.
sair() -> exit(self(), {ok, saiu_com_sucesso}).
