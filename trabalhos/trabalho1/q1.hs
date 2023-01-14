module Q1_trab1 where

primo(x, y)
    | y > div x 2 = x
    | mod x y == 0 = primo(x+1, 2)
    | otherwise = primo(x, y+1)

calcula(a, b, c, p)
    | mod a p == 0 && mod b p == 0 && mod c p == 0 = p : calcula(div a p, div b p, div c p, p)
    | mod a p == 0 && mod b p == 0 = p : calcula(div a p, div b p, c, p)
    | mod a p == 0 && mod c p == 0 = p : calcula(div a p, b, div c p, p)
    | mod b p == 0 && mod c p == 0 = p : calcula(a, div b p, div c p, p)
    | mod a p == 0 = p : calcula(div a p, b, c, p)
    | mod b p == 0 = p : calcula(a, div b p, c, p)
    | mod c p == 0 = p : calcula(a, b, div c p, p)
    | otherwise = calcula(a, b, c, primo(p+1, 2))

fatoracao(a, b, c) = calcula(a, b, c, 2)

