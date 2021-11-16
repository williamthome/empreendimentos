-module(menu).

-export([
  imprimir_menu/1,
  pegar_id_de_opcao_do_usuario/0, pegar_opcao_por_id/2,
  executar_acao_da_opcao_por_id/3
]).

-define(SEPARADOR, "----------------------------------").
-define(CABECALHO, "Menu de opções dos Empreendimentos").
-define(PERGUNTA, "O que deseja fazer?").
-define(ERRO_OPCAO_NAO_ENCONTRADA, {erro, opcao_nao_encontrada}).

imprimir_menu(Opcoes) ->
  utils:imprimir_string(?SEPARADOR),
  utils:imprimir_string(?CABECALHO),
  utils:imprimir_string(?SEPARADOR),
  imprimir_opcoes(Opcoes),
  utils:imprimir_string(?SEPARADOR).

imprimir_opcoes(Opcoes) ->
  lists:foreach(
    fun utils:imprimir_string/1,
    opcoes_com_id_e_nome_concatenados(Opcoes)
  ).

opcoes_com_id_e_nome_concatenados(Opcoes) ->
  lists:map(fun concatenar_id_e_nome_de_opcao/1, Opcoes).

concatenar_id_e_nome_de_opcao([{id, Id}, {nome, Nome}, _Fun]) ->
  lists:concat([Id, ": ", Nome]).

pegar_id_de_opcao_do_usuario() ->
  Resposta = utils:perguntar_ao_usuario(?PERGUNTA),
  try list_to_integer(Resposta) of
    Id -> {ok, Id}
  catch
    error:badarg ->
      {erro, opcao_deve_ser_um_inteiro}
  end.

pegar_opcao_por_id(Id, Opcoes) ->
  case tentar_econtrar_opcao_por_id(Id, Opcoes) of
    {value, Opcao} -> {ok, Opcao};
    false -> ?ERRO_OPCAO_NAO_ENCONTRADA
  end.

tentar_econtrar_opcao_por_id(Id, Opcoes)
  when is_integer(Id) ->
    lists:search(fun(Opcao) -> opcao_possui_o_id(Id, Opcao) end, Opcoes).

opcao_possui_o_id(Id, [{id, Id}, _Nome, _Fun]) -> true;
opcao_possui_o_id(_IdOpcao, [_Id, _Nome, _Fun]) -> false.

executar_acao_da_opcao_por_id(Id, Opcoes, Empreendimentos) ->
  case pegar_opcao_por_id(Id, Opcoes) of
    {ok, [_Id, {nome, NomeDaOpcao}, {acao, Fun}]} ->
      io:format("Executando ~s...~n", [NomeDaOpcao]),
        Fun(Empreendimentos);
    ?ERRO_OPCAO_NAO_ENCONTRADA ->
      ?ERRO_OPCAO_NAO_ENCONTRADA
  end.
