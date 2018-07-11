# htxt

## Compilación

Para la compilación de la aplicación se utilizó la herramienta Stack que facilita la administración de dependencias en Haskell.

El comando ‘stack build’ permite compilar el programa.

## Ejecución

Para correr la aplicación sin que el binario sea agregado a carpeta bin local del usuario, se debe correr el comando "stack exec htxt <filepath>" donde el parametro filepath es el path desde la posición actual hacia el archivo que se desea abrir. En caso de que no exista ese archivo, se creará uno nuevo.

## Instalación

Para instalar htxt y que el comando pueda ser accedido desde cualquier directorio, se debe correr el comando "stack install". Este comando compila la aplicación y mueve el archivo binario a la carpeta de binarios del usuario. Una vez instalado, se puede acceder a la aplicación mediante el comando “htxt <filepath>” donde filepath es el archivo que se quiere modificar o crear.
