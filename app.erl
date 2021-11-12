-module(app).
-export([iniciar/0]).
-define(SERVIDOR, ?MODULE).
-define(SEPARADOR, "----------------------------------").
-define(CABECALHO, "Menu de opções dos Empreendimentos").
-define(OPCOES, [
  {{id, 1}, {nome, "Listar"}, {acao, fun() -> ok end}},
  {{id, 2}, {nome, "Adicionar"}, {acao, fun() -> ok end}},
  {{id, 3}, {nome, "Buscar"}, {acao, fun() -> ok end}},
  {{id, 4}, {nome, "Editar"}, {acao, fun() -> ok end}},
  {{id, 5}, {nome, "Deletar"}, {acao, fun() -> ok end}},
  {{id, 6}, {nome, "Sair"}, {acao, fun() -> exit(whereis(?SERVIDOR), ok) end}}
]).
-define(PERGUNTA, "O que deseja fazer?").

iniciar() ->
  imprimir_menu_de_opcoes(),
  Resposta = pegar_resposta_do_usuario(),
  io:format("Resposta: ~s~n", [Resposta]),
  iniciar().

imprimir_menu_de_opcoes() ->
  io:format("~s~n", [?SEPARADOR]),
  io:format("~s~n", [?CABECALHO]),
  io:format("~s~n", [?SEPARADOR]),
  lists:foreach(fun imprimir_item_do_menu/1, pegar_menu_de_opcoes()),
  io:format("~s~n", [?SEPARADOR]).

imprimir_item_do_menu(Item) ->
  io:format("~s~n", [Item]).

pegar_menu_de_opcoes() ->
  lists:map(fun opcao_para_item_de_menu/1, ?OPCOES).

opcao_para_item_de_menu({{id, Id}, {nome, Nome}, _Fun}) ->
  lists:concat([Id, ": ", Nome]).

pegar_resposta_do_usuario() ->
  Pergunta = io_lib:format("~s ", [?PERGUNTA]),
  io:get_line(Pergunta).
