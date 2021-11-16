-module(app).

-export([iniciar/0]).

-define(OPCOES, [
  [{id, 1}, {nome, "Listar"}, {acao, fun repo:listar/1}],
  [{id, 2}, {nome, "Adicionar"}, {acao, fun repo:adicionar/1}],
  [{id, 3}, {nome, "Buscar"}, {acao, fun repo:buscar/1}],
  [{id, 4}, {nome, "Editar"}, {acao, fun repo:editar/1}],
  [{id, 5}, {nome, "Deletar"}, {acao, fun repo:deletar/1}],
  [{id, 6}, {nome, "Sair"}, {acao, fun sair/1}]
]).
-define(ERRO_NO_SERVIDOR, {erro, algo_inesperado_aconteceu}).

iniciar() ->
  iniciar([]).

iniciar(Empreendimentos)
  when is_list(Empreendimentos) ->
    menu:imprimir_menu(?OPCOES),
    case menu:pegar_id_de_opcao_do_usuario() of
      {ok, IdOpcao} ->
        case menu:executar_acao_da_opcao_por_id(IdOpcao, ?OPCOES, Empreendimentos) of
          {ok, {Resposta, EmpreendimentosDaAcao}}
            when is_list(EmpreendimentosDaAcao) ->
              utils:imprimir_resposta(Resposta),
              utils:presisonar_enter_para_continuar(),
              iniciar(EmpreendimentosDaAcao);
          {erro, Erro} ->
            utils:imprimir(Erro),
            utils:presisonar_enter_para_continuar(),
            iniciar(Empreendimentos);
          _ ->
            utils:imprimir(?ERRO_NO_SERVIDOR),
            utils:presisonar_enter_para_continuar(),
            iniciar(Empreendimentos)
        end;
      Erro ->
        utils:imprimir(Erro),
        utils:presisonar_enter_para_continuar(),
        iniciar(Empreendimentos)
    end.

sair(Empreendimentos)
  when is_list(Empreendimentos) ->
    Resposta = saiu_com_sucesso,
    exit(self(), {ok, {{sair, Resposta}, Empreendimentos}}).
