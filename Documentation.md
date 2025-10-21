# Estructuras de datos en haskell: Árboles binarios

Aquí presento el cómo diseñé el módulo de BinaryTree.hs (en la raíz del documento), así como
la forma básica de cómo está organizado.

## Notación

En lugar de "Árbol", decidí llamar al objeto como "Nodo". Se me hizo más apropiado que un Nodo tenga SubNodos y en su conjunto
se llamen "Árbol", a diferencia de hacer un Arbol y que deriven Subárboles y que en su conjunto sean "Árboles (?) (Bosque?)".

## Uso de Maybe en lugar de null

En el ejemplo de la profe, usamos algo así:

```haskell
-- Esta forma de declararlo es ambigua, no tiene nombres y es como si fuera: Null | (int, node?, node?) de tupla en c#, no tiene ni nombres
-- por lo que el código será más difícil de leer, tendremos que recordar el orden de sus campos...
data Node = Null | Node Integer, Node, Node
```

Pero preferí ponerle nombres, como los objetos en C#

```haskell
data Node = Null | Node {
  value     :: Integer,
  leftNode  :: Node,
  rightNode :: Node
}
```

y en lugar de que sea Null | Node, es mejor declararlo como algo fijo y que solamente sus propiedades puedan ser "null"

```haskell
data Node = Node {
  value     :: Integer,
  leftNode  :: Maybe Node,
  rightNode :: Maybe Node
}
```

El Maybe es un tipo de dato que creo que está declarado así en haskell:

```haskell
data Maybe a = Nothing | Just a
```

Significa que un tipo de dato maybe puede ser literalmente un "Nothing" o un objeto Just que incluye. Osea, puede ser "null" o el objeto en sí,
similar a Node? en c# o a (Node | Null) en typescript, el cual su equivalente sería:

```ts
//Si 🦊
type Node = {
  value: number;
  leftNode: Node | null;
  rightNode: Node | null;
};
```

El cual, en mi experiencia, es mucho más limpio y te ahorra tener que andar depurando el programa a como si lo hicieras:

```ts
//Esto no
type Node =
  | Null
  | {
      value: number;
      leftNode: Node;
      rightNode: Node;
    };
```

Basado en todo el tiempo que llevo programando en typescript (y c# supongo)

## Diseño del móculo "BinaryTree.hs"

Todos las funciones están nombradas de la siguiente manera con "node####" para hacer referencia que vienen del mismo archivo, así
también el intelissense nos ayuda para diferenciar de diferentes imports, con colocar solamente 'node...' ya podemos ver todas las funciones directamente en el intelissense que provengan del módulo. (Es lo más similar al intelissense de la nomenclatura del "."
cuando se usan objetos en C# por ejemplo)

Las equivalencias con C# y haskell son las siguientes para entender mejor los nombres de las funciones:

Así se vería el código en C#

```csharp
  // Esta clase tiene 3 constructores y el método insertar.
  class Nodo{
    public int value;
    public Nodo? leftNode;
    public Nodo? rightNode;

    public Nodo(){ // Equivalente a 'nodeCreateEmpty' de haskell (BinaryTree.hs)
      this.value = 0;
      this.leftNode = null;
      this.rightNode = null;
    }

    public Nodo(int initialValue){ // Equivalente a 'nodeCreateSimpleWithValue' de haskell (BinaryTree.hs)
      this.value = initialValue;
      this.leftNode = null;
      this.rightNode = null;
    }

    public Nodo(int[] initialValues){
      foreach(int value in initialValues){
        this.Insert(value); // Equivalente a 'nodeInsertInto' de haskell (BinaryTree.hs)
      }
    }
  }
  ///.. en otra parte del código
  var miNodo = new Nodo();
  miNodo.HacerAlgo();
  Console.WriteLine(miNodo.value); // 0
  Console.WriteLine(miNodo.leftNode ?? "es nulo"); //"es nulo;
  Console.WriteLine(miNodo.rightNode ?? "es nulo"); //"es nulo;
```

El equivalente sería así en haskell:

```haskell
-- Es la definición de la "clase" (del la estructura de datos que representa un "Nodo")
data Node = Node {
  value     :: Integer,    -- El valor del nodo, igual que en c#
  leftNode  :: Maybe Node, -- Es lo mismo que Node? del c#, puede ser null ('Nothing' en haskell) or un nodo ('Just' Node)
  rightNode :: Maybe Node  -- Es lo mismo que Node? del c#, puede ser null ('Nothing' en haskell) or un nodo ('Just' Node)
}

-- #1 Constructor Node(int initialValue) como en C#
nodeCreateSimpleWithValue :: Integer -> Node
nodeCreateSimpleWithValue value = Node{value=value, leftNode=Nothing, rightNode=Nothing}

-- #2 Constructor Node() como en c#
nodeCreateEmpty :: Node
nodeCreateEmpty = nodeCreateSimpleWithValue 0

-- #3 Constructor Node(int[] initialValues) como en c#
nodeCreateFromList :: [Integer] -> Node
nodeCreateFromList [] = nodeCreateEmpty
nodeCreateFromList (x:xs) =
    let root = nodeCreateSimpleWithValue x
        in foldl nodeInsertInto root xs

-- ... en otra parte del código
main :: IO()
main = do
  let miNodo :: Node = nodeCreateEmpty
  putStrLn $ value miNodo

  case leftNode miNodo of
    Just node -> do
      putStrLn $ nodeToString node
    Nothing -> do
      putStrLn "es nulo"

  case rightNode miNodo of
    Just node -> do
      putStrLn $ nodeToString node
    Nothing -> do
      putStrLn "es nulo"
```

## Diseño del módulo: "BinaryTreeDefaults.hs"

Este es un pequeño archivo que cree solamente para tener árboles pre-hechos ya con varios valores para poder mostrar
como ejemplo dentro del menú que haríamos en la actividad.
