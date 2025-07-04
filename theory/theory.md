# Theory problems


## Type analysis

### bool?

Можно было бы добавить bool с булевыми литералами, тогда условие `if` ноды бы типизировалось булем: $$if (E) { S }, ||E|| = bool $$,
и условие под `while` - тоже $bool$.
Можно ввести новые операторы (`&&`, `||`, etc).

Потребовался бы другой, bool-специфичный `input`/`output` для случаев, когда невозможно вывести тип: bool/int из последующих использований.
(ну или не потребовался, если неявно преобразовывать int -> bool, но тогда не совсем понятно, зачем вообще вводить буль с точки зрения анализа типов)

- Область типизируемых программ уменьшится
- Область slack расширится
- Полнота (recall) анализа (если считать, что программа с интом в условии - это окэй в рантайме) не изменится, т.к. будет обнаружено больше **неистинных** результатов,
=> количество обнаруженных результатов среди истинных останется таким же.
- Точность (precision) анализа уменьшится, т.к. на то же число истинных результатов, будет больше полученных результатов.

### array?

Добавление массива в систему типов (в какой-то степени схоже с функциями, в которой аргумент - Int):
$$a[X] = E : [[a]] = Arr[[E]], [[X]] = int$$
$$a[X]: [[a]] = Arr[[a[x]]] \& [[X]] = int$$ или для простоты можно считать, что элемент массива - число: $ a[x] = int \& [[x]] = int $
$$ a = {}: [[a]] = Arr[[\$NewVar]] $$
$$ a = { X1, X2, ..., Xn }; [[a]] = Arr[[X1]] \& [[X1]] = [[X2]] \& ... \& [[Xm]] = [[Xn]] $$

### typing

> Попробуйте протипизировать программу со слайда

```tip
main() {
    var x,y,z,t;            // xyzt
    x = {2,4,8,16,32,64};   // [[x]] = Arr[int], [[2]] = int, ...
    y = x[x[3]];            // [[x[3]]] = int, [[x]] = Arr[int], [[y]] = int
    z = {{},x};             // [[{}]] = Arr[int], [[z]] = Arr[Arr[int]]
    t = z[1];               // [[z]] = Arr[?] (? = Arr[int]), [[t]] = Arr[int]
    t[2] = y;               // [[t]] = Arr[[y]] == Arr[[int]]
}
```

## lattices

> Давайте попробуем определить статически знак
выражения в TIP’е?  
> А почему нам не подходит конкретный домен?

- Нужно уметь вычислять знак в зависимости от операции и абсолютных значений, иначе решетка будет бесконечной.

> Можно ли выразить анализ типов с предыдущей лекции как анализ над решетками?

Кажется, что частично. 

$$ \top (Any) $$
$$ int, \{int\}, \{int, ..., int\},  \uparrow int, \uparrow\uparrow int ... $$
$$ \bot (Nothing) $$

Кажется, что решетка получится бесконечной в ширину хотя бы из-за указателей или записей.

## variable size analysis

Можно использовать интервальный анализ с заданным *B*-интервалами по допустимым значениям переменных конкретных типов:
```scala
    val Bool = TipTypeVal(0, 1)
    val Byte = TipTypeVal(-128, 127)
    val Char = TipTypeVal(0, 65535)
    val Int = TipTypeVal(-2147483648, 2147483647)
    val BigInt = TipTypeVal(MInf, PInf)
//  val Any = TipTypeVal(MInf, PInf) ???
```

Переменной соответствует тип (и размер), наименьший из вмещающих *B* интерваловю

$$\top = BigInt(-\infty, +\infty) -\infty bits$$

$$\uparrow$$

$$ Int[-2 147 483 648, 2 147 483 647] - 32 bits $$

$$\uparrow$$

$$ Char[0, 65 535] - 16 bits $$

$$\uparrow$$

$$ Byte[-128, 127] - 8 bits $$

$$\uparrow$$

$$ Bool[0, 1] - 1 bit $$

$$\uparrow$$

$$ \bot $$


- `T(E)` - оператор приведения. Если он нужен только для оптимизирующего компилятора, то это и есть выбор минимально подходящего 
типа по допустимому интервалу значений. Если этот оператор добавить как конструкцию языка, то не очень понятно, как описать конкретные значения
в случае переполнения на решетке.


## Path sensitivity - open/close

Example on how to trick open-close analysis

```tip
var p, x, y;
&x = p; // We don't have pointer analysis now, right?
&y = p;
x = 1;
y = 2; // x != y, right?...

if (x == 1) {
    open(); // ha-ha
}

if (y == 2) {
    close(); // ooops...
}
```

### Solution

Ввести анализ указателей и определять, что под разными переменными могут быть одни и те же адреса

## Interprocedural analysis

> Полный функциональный вид контекстной чувствительности
является строго наиболее точным из возможных  
• Это значит, что он так же точен, как и “полный” инлайнинг  
• И то, что мы никогда не анализируем недостижимые пути по вызовам  
• Попробуйте доказать этот факт самостоятельно

- Так мы считаем summary, только если входим в конкретный вызов. Если вызов недостижим, для функции summary не считается

### context-sensetive analysis with k > 1
Пример:
```tip
f(x) {
if (x > 0) {
    return f(-x);
} else {
    return x * 10;
}
}

main() {
    f(123);    
}
```

> Приведите пример решётки, для которой контекстно-чувствительный анализ в функциональном стиле
является более ресурсозатратным, чем контекстно-чувствительный анализ по месту вызова с глубиной 2

- Если количество аргументов и переменных больше, чем количество вызовов функции. Для data-driven context sensetivity требуется
состояние из квадрата по количеству переменных, для анализа по месту вызова - квадрат мест, откуда вызывается функция?

