-module(repo).

-export([
  listar/1,
  adicionar/1,
  buscar/1,
  editar/1,
  deletar/1
]).

-define(ERRO_EMPREENDIMENTO_NAO_ENCONTRADO, {erro, empreendimento_nao_encontrado}).

-record(empreendimento, {
  nome :: nonempty_string()
}).

listar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Resposta = case Empreendimentos of
      [] -> nenhum_empreendimento_cadastrado;
      _ -> Empreendimentos
    end,
    {ok, {{listar, Resposta}, Empreendimentos}}.

adicionar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Nome = utils:perguntar_ao_usuario("Informe o nome do empreendimento"),
    Empreendimento = #empreendimento{nome = Nome},
    Resposta = Empreendimento,
    {ok, {{adicionar, Resposta}, [Empreendimento | Empreendimentos]}}.

buscar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Nome = utils:perguntar_ao_usuario(
      "Informe o nome do empreendimento que deseja buscar"
    ),
    Resposta = pegar_empreendimento_por_nome(Empreendimentos, Nome),
    {ok, {{buscar, Resposta}, Empreendimentos}}.

editar(Empreendimentos)
  when is_list(Empreendimentos) ->
    Nome = utils:perguntar_ao_usuario(
      "Informe o nome do empreendimento que deseja editar"
    ),
    case pegar_empreendimento_por_nome(Empreendimentos, Nome) of
      {ok, _Empreendimento} ->
        NovoNome = utils:perguntar_ao_usuario("Informe o novo nome do empreendimento"),
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
     Nome = utils:perguntar_ao_usuario(
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
