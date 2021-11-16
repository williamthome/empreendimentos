-module(app).
-export([iniciar/0]).
-define(SERVIDOR, ?MODULE).
-define(SEPARADOR, "----------------------------------").
-define(CABECALHO, "Menu de opções dos Empreendimentos").
-define(OPCOES, [
  [{id, 1}, {nome, "Listar"}, {acao, fun listar/1}],
  [{id, 2}, {nome, "Adicionar"}, {acao, fun adicionar/1}],
  [{id, 3}, {nome, "Buscar"}, {acao, fun buscar/1}],
  [{id, 4}, {nome, "Editar"}, {acao, fun editar/1}],
  [{id, 5}, {nome, "Deletar"}, {acao, fun deletar/1}],
  [{id, 6}, {nome, "Sair"}, {acao, fun sair/1}]
]).
-define(PERGUNTA, "O que deseja fazer?").
-define(ERRO_OPCAO_NAO_ENCONTRADA, {erro, opcao_nao_encontrada}).
-define(ERRO_NO_SERVIDOR, {erro, algo_inesperado_aconteceu}).
-define(ERRO_EMPREENDIMENTO_NAO_ENCONTRADO, {erro, empreendimento_nao_encontrado}).
-record(empreendimento, {
  nome :: nonempty_string()
}).

iniciar() ->
  iniciar([]).

iniciar(Empreendimentos)
  when is_list(Empreendimentos) ->
    imprimir_menu(),
    case pegar_id_de_opcao_do_usuario() of
      {ok, IdOpcao} ->
        Resultado = case pegar_opcao_por_id(IdOpcao) of
          {ok, [_Id, {nome, NomeDaOpcao}, {acao, Fun}]} ->
            io:format("Executando ~s...~n", [NomeDaOpcao]),
            Fun(Empreendimentos);
          ?ERRO_OPCAO_NAO_ENCONTRADA ->
            ?ERRO_OPCAO_NAO_ENCONTRADA
        end,
        case Resultado of
          {ok, {Resposta, EmpreendimentosDaAcao}}
            when is_list(EmpreendimentosDaAcao) ->
              imprimir_resposta(Resposta),
              presisonar_enter_para_continuar(),
              iniciar(EmpreendimentosDaAcao);
          ?ERRO_OPCAO_NAO_ENCONTRADA ->
            imprimir(?ERRO_OPCAO_NAO_ENCONTRADA),
            presisonar_enter_para_continuar(),
            iniciar(Empreendimentos);
          _ ->
            imprimir(?ERRO_NO_SERVIDOR),
            presisonar_enter_para_continuar(),
            iniciar(Empreendimentos)
        end;
      Erro ->
        imprimir(Erro),
        presisonar_enter_para_continuar(),
        iniciar(Empreendimentos)
    end.

presisonar_enter_para_continuar() ->
  io:get_line("Pressione enter para voltar ao menu...").

imprimir(Valor) ->
    io:format("~p~n", [Valor]).

imprimir_string(Valor)
  when is_list(Valor) ->
    io:format("~s~n", [Valor]).

imprimir_quebra() ->
    io:format("~n").

imprimir_menu() ->
  imprimir_string(?SEPARADOR),
  imprimir_string(?CABECALHO),
  imprimir_string(?SEPARADOR),
  imprimir_opcoes(),
  imprimir_string(?SEPARADOR).

imprimir_opcoes() ->
  lists:foreach(fun imprimir_string/1, opcoes_com_id_e_nome_concatenados()).

imprimir_resposta(Resposta) ->
  imprimir_string(?SEPARADOR),
  imprimir_string("Resultado:"),
  imprimir_quebra(),
  imprimir(Resposta),
  imprimir_string(?SEPARADOR).

opcoes_com_id_e_nome_concatenados() ->
  lists:map(fun concatenar_id_e_nome_de_opcao/1, ?OPCOES).

concatenar_id_e_nome_de_opcao([{id, Id}, {nome, Nome}, _Fun]) ->
  lists:concat([Id, ": ", Nome]).

perguntar_ao_usuario(Pergunta) ->
  PerguntaFormatada = io_lib:format("~s ", [Pergunta]),
  string:trim(io:get_line(PerguntaFormatada)).

pegar_id_de_opcao_do_usuario() ->
  Resposta = perguntar_ao_usuario(?PERGUNTA),
  try list_to_integer(Resposta) of
    Id -> {ok, Id}
  catch
    error:badarg ->
      {erro, opcao_deve_ser_um_inteiro}
  end.

pegar_opcao_por_id(Id) ->
  case tentar_econtrar_opcao_por_id(Id, ?OPCOES) of
    {value, Opcao} -> {ok, Opcao};
    false -> ?ERRO_OPCAO_NAO_ENCONTRADA
  end.

tentar_econtrar_opcao_por_id(Id, Opcoes)
  when is_integer(Id) ->
    lists:search(fun(Opcao) -> opcao_possui_o_id(Id, Opcao) end, Opcoes).

opcao_possui_o_id(Id, [{id, Id}, _Nome, _Fun]) -> true;
opcao_possui_o_id(_IdOpcao, [_Id, _Nome, _Fun]) -> false.

listar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Resposta = case Empreendimentos of
      [] -> nenhum_empreendimento_cadastrado;
      _ -> Empreendimentos
    end,
    {ok, {{listar, Resposta}, Empreendimentos}}.

adicionar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Nome = perguntar_ao_usuario("Informe o nome do empreendimento"),
    Empreendimento = #empreendimento{nome = Nome},
    Resposta = Empreendimento,
    {ok, {{adicionar, Resposta}, [Empreendimento | Empreendimentos]}}.

buscar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Nome = perguntar_ao_usuario(
      "Informe o nome do empreendimento que deseja buscar"
    ),
    Resposta = pegar_empreendimento_por_nome(Empreendimentos, Nome),
    {ok, {{buscar, Resposta}, Empreendimentos}}.

editar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Nome = perguntar_ao_usuario(
      "Informe o nome do empreendimento que deseja editar"
    ),
    case pegar_empreendimento_por_nome(Empreendimentos, Nome) of
      {ok, _Empreendimento} ->
        NovoNome = perguntar_ao_usuario("Informe o novo nome do empreendimento"),
        EmpreendimentosFiltrados = deletar_empreendimento_por_nome(
          Empreendimentos,
          Nome
        ),
        Empreendimento = #empreendimento{nome = NovoNome},
        EmpreendimentosAtualizados = [Empreendimento | EmpreendimentosFiltrados],
        Resposta = Empreendimento,
        {ok, {{editar, Resposta}, EmpreendimentosAtualizados}};
      ?ERRO_EMPREENDIMENTO_NAO_ENCONTRADO ->
        Resposta = ?ERRO_EMPREENDIMENTO_NAO_ENCONTRADO,
        {ok, {{editar, Resposta}, Empreendimentos}}
    end.

deletar(Empreendimentos)
  when is_list(Empreendimentos) ->
     Nome = perguntar_ao_usuario(
      "Informe o nome do empreendimento que deseja deletar"
    ),
    case pegar_empreendimento_por_nome(Empreendimentos, Nome) of
      {ok, _Empreendimento} ->
        EmpreendimentosAtualizados = deletar_empreendimento_por_nome(
          Empreendimentos,
          Nome
        ),
        Resposta = EmpreendimentosAtualizados,
        {ok, {{deletar, Resposta}, EmpreendimentosAtualizados}};
      ?ERRO_EMPREENDIMENTO_NAO_ENCONTRADO ->
        Resposta = ?ERRO_EMPREENDIMENTO_NAO_ENCONTRADO,
        {ok, {{deletar, Resposta}, Empreendimentos}}
    end.

sair(Empreendimentos)
  when is_list(Empreendimentos) ->
    Resposta = saiu_com_sucesso,
    exit(self(), {ok, {{sair, Resposta}, Empreendimentos}}).

pegar_empreendimento_por_nome(Empreendimentos, Nome) ->
  case tentar_encontrar_empreendimento_por_nome(Empreendimentos, Nome) of
    {value, Empreendimento} -> {ok, Empreendimento};
    false -> ?ERRO_EMPREENDIMENTO_NAO_ENCONTRADO
  end.

tentar_encontrar_empreendimento_por_nome(Empreendimentos, Nome)
  when is_list(Empreendimentos) ->
    lists:search(
      fun(#empreendimento{nome = NomeDoEmpreendimento}) ->
        NomeDoEmpreendimento == Nome end,
      Empreendimentos
    ).

deletar_empreendimento_por_nome(Empreendimentos, Nome)
  when is_list(Empreendimentos) ->
    lists:filter(
      fun(#empreendimento{nome = NomeDoEmpreendimento}) ->
        NomeDoEmpreendimento =/= Nome end,
      Empreendimentos
    ).
