-module(utils).

-export([
  presisonar_enter_para_continuar/0,
  imprimir/1, imprimir_string/1, imprimir_quebra/0,
  perguntar_ao_usuario/1,
  imprimir_e_aguardar_enter/1,
  imprimir_resposta/1
]).

presisonar_enter_para_continuar() ->
  io:get_line("Pressione enter para voltar ao menu...").

imprimir(Valor) ->
    io:format("~p~n", [Valor]).

imprimir_string(Valor)
  when is_list(Valor) ->
    io:format("~s~n", [Valor]).

imprimir_quebra() ->
    io:format("~n").

perguntar_ao_usuario(Pergunta) ->
  PerguntaFormatada = io_lib:format("~s ", [Pergunta]),
  string:trim(io:get_line(PerguntaFormatada)).

imprimir_e_aguardar_enter(Valor) ->
  imprimir(Valor).

imprimir_resposta(Resposta) ->
  Separador = "----------------------------------",
  imprimir_string(Separador),
  imprimir_string("Resultado:"),
  imprimir_quebra(),
  imprimir(Resposta),
  imprimir_string(Separador).
