#!/usr/bin/env python3

from __future__ import annotations
from dataclasses import dataclass
from typing import Union, Optional, Tuple, List, Dict, Set
import re
import sys
from pathlib import Path
import time

# Tipos personalizados para melhor legibilidade
Expression = Union['LambdaExpression', 'Application', str]

@dataclass
class LambdaExpression:
    """Representa uma expressão lambda (abstração) com uma variável e um corpo."""
    var: str
    body: Optional[Expression] = None

    def __call__(self, arg: Expression) -> Expression:
        """Aplica a expressão lambda a um argumento através de substituição."""
        return substitute(self.body, self.var, arg)

    def __repr__(self) -> str:
        if self.body is None:
            return self.var
        return f"λ{self.var}. {self.body}"

@dataclass
class Application:
    """Representa a aplicação de uma função a um argumento."""
    func: Expression
    arg: Expression

    def __repr__(self) -> str:
        return f"({self.func} {self.arg})"

class Environment:
    """Mantém um ambiente com as definições de funções."""
    def __init__(self):
        self.definitions: Dict[str, Expression] = {}

    def define(self, name: str, expr: Expression) -> None:
        """Define uma nova função no ambiente."""
        self.definitions[name] = expr

    def lookup(self, name: str) -> Optional[Expression]:
        """Procura uma definição no ambiente."""
        return self.definitions.get(name)

class LambdaParseError(Exception):
    """Exceção personalizada para erros de parsing."""
    pass

def substitute(expr: Expression, var: str, value: Expression) -> Expression:
    """Realiza a substituição de variáveis em uma expressão lambda."""
    match expr:
        case LambdaExpression(v, body) if v == var:
            return expr
        case LambdaExpression(v, body):
            return LambdaExpression(v, substitute(body, var, value))
        case Application(func, arg):
            return Application(
                substitute(func, var, value),
                substitute(arg, var, value)
            )
        case str() if expr == var:
            return value
        case _:
            return expr

def tokenize(expr: str) -> List[str]:
    """Converte uma string em uma lista de tokens."""
    # Adiciona espaços ao redor de caracteres especiais
    expr = re.sub(r'([λ\.\(\)=])', r' \1 ', expr)

    # Remove espaços extras
    expr = re.sub(r'\s+', ' ', expr).strip()

    invalid_chars = re.findall(r'[^λ\.\(\)\w\s=]', expr)
    if invalid_chars:
        raise LambdaParseError(f"Caracteres inválidos encontrados: {invalid_chars}")

    return [token for token in expr.split() if token.strip()]

def parse_application(tokens: List[str], pos: int) -> Tuple[Expression, int]:
    """Parse uma aplicação de função."""
    func, pos = parse_expression(tokens, pos)
    expressions = [func]

    while pos < len(tokens) and tokens[pos] != ")":
        expr, new_pos = parse_expression(tokens, pos)
        expressions.append(expr)
        pos = new_pos

    if not expressions:
        raise LambdaParseError("Aplicação vazia")

    result = expressions[0]
    for expr in expressions[1:]:
        result = Application(result, expr)

    return result, pos

def parse_definition(tokens: List[str], pos: int) -> Tuple[str, Expression, int]:
    """Parse uma definição de função usando 'let'."""
    if pos + 2 >= len(tokens) or tokens[pos + 2] != "=":
        raise LambdaParseError("Sintaxe inválida para definição let")

    name = tokens[pos + 1]
    expr, new_pos = parse_expression(tokens, pos + 3)
    return name, expr, new_pos

def parse_expression(tokens: List[str], pos: int) -> Tuple[Expression, int]:
    """Parse uma expressão lambda."""
    if pos >= len(tokens):
        raise LambdaParseError("Fim inesperado da expressão")

    current = tokens[pos]

    if current == "λ":
        if pos + 2 >= len(tokens) or tokens[pos + 2] != ".":
            raise LambdaParseError("Sintaxe inválida para expressão lambda")

        var = tokens[pos + 1]
        if not var.isalnum():
            raise LambdaParseError(f"Nome de variável inválido: {var}")

        body, new_pos = parse_expression(tokens, pos + 3)
        return LambdaExpression(var, body), new_pos

    elif current == "(":
        expr, pos = parse_application(tokens, pos + 1)

        if pos >= len(tokens) or tokens[pos] != ")":
            raise LambdaParseError("Parêntese não fechado")

        return expr, pos + 1

    elif current == ")" or current == "." or current == "=":
        raise LambdaParseError(f"Token inesperado: {current}")

    return current, pos + 1

def evaluate(expr: Expression, env: Environment, used_defs: Set[str], max_steps: int = 1000) -> Tuple[Expression, int]:
    """Avalia uma expressão lambda até atingir sua forma normal."""
    steps = 0
    while steps < max_steps:
        if isinstance(expr, str):
            defined_expr = env.lookup(expr)
            if defined_expr is not None:
                used_defs.add(expr)
                expr = defined_expr
                continue
            return expr, steps

        if not isinstance(expr, Application):
            return expr, steps

        func, func_steps = evaluate(expr.func, env, used_defs)
        arg, arg_steps = evaluate(expr.arg, env, used_defs)

        steps += func_steps + arg_steps

        if isinstance(func, LambdaExpression):
            expr = evaluate(func(arg), env, used_defs)[0]
        else:
            return Application(func, arg), steps

        steps += 1

    raise RuntimeError("Número máximo de passos de redução excedido")

def process_code(code: str) -> Tuple[Expression, Environment]:
    """Processa o código fonte, executando definições e retornando o resultado da última expressão."""
    env = Environment()
    lines = [line.strip() for line in code.split('\n') if line.strip()]
    last_expr = None

    for line in lines:
        tokens = tokenize(line)
        if not tokens:
            continue

        if tokens[0] == "let":
            name, expr, _ = parse_definition(tokens, 0)
            env.define(name, expr)
        else:
            expr, _ = parse_expression(tokens, 0)
            last_expr = expr

    if last_expr is None:
        raise LambdaParseError("Nenhuma expressão para avaliar")

    return last_expr, env

def main() -> None:
    """Função principal do interpretador."""
    if len(sys.argv) < 2:
        print("Uso: python3 lambda_interpreter.py <arquivo> [-s] [-c]")
        sys.exit(1)

    file_path = Path(sys.argv[1])
    show_stats = "-s" in sys.argv
    show_context = "-c" in sys.argv

    try:
        if not file_path.exists():
            raise FileNotFoundError(f"Arquivo não encontrado: {file_path}")

        code = file_path.read_text()
        start_time = time.time()
        last_expr, env = process_code(code)
        used_defs = set()
        result, steps = evaluate(last_expr, env, used_defs)
        end_time = time.time()
        elapsed_time = end_time - start_time

        print("Resultado final:")
        print("=>", result)
        if show_stats:
            print("==================================================")
            print(f"Tempo de execução: {elapsed_time:.6f} segundos")
            print(f"Número de passos de redução: {steps}")
        if show_context:
            print("==================================================")
            print("Definições utilizadas:")
            for name in used_defs:
                print(f"{name} = {env.lookup(name)}")

    except LambdaParseError as e:
        print(f"Erro de parsing: {e}")
        sys.exit(1)
    except FileNotFoundError as e:
        print(f"Erro de arquivo: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Erro inesperado: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
