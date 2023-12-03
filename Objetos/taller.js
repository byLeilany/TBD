// Ejercicio 1
function ejercicio1() {
  Malambo = {
      nombre : "Malambo",
      peso : 500,
      directiva : "limpiar"
    };
}

// Ejercicio 2
function ejercicio2() {
  nuevoRobot = function(n, w, a) {
    let res = Object.create(Malambo)
    res.nombre = n
    res.peso = w 
    res.directiva = a
    return res
  };

  Chacarera = nuevoRobot("Chacarera", 1500, "cortar el pasto");
}

// Ejercicio 3
function ejercicio3() {
  Malambo.presentarse = function(){
                          return "Hola, soy " + this.nombre + " y me encanta " + this.directiva + "."
                        }

  Malambo.limpiar = function() {
    this.peso += 1
    return "limpiar"
  }
}

// Ejercicio 4
function ejercicio4() {
  Robot = function(n,p,d,f){
    this.nombre = n
    this.peso = p
    this.directiva = d
    this[d] = f
  };
  Robot.prototype.presentarse = Malambo.presentarse
}

// Ejercicio 5
function ejercicio5() {
  let f = function(remit, dest, sms){

    if(dest[sms] != undefined){

      let respuesta = dest[sms]()

      if(remit[respuesta] != undefined){
        return remit[respuesta]()
      }else{
        return respuesta
      }

    }else{
      return sms
    } 
  }

  Milonga = new Robot("Milonga", 1200, "mensajear", f);
}

// Ejercicio 6
/*
call: cuando armo robotSMS con el call llamo a la funcion constructora de Robot con los argumentos n p d y se los asignos ahora al this en RobotSMS
*/
function ejercicio6() {
  RobotMensajero = function(nombre, peso, directiva) {
    Robot.call(this, nombre, peso, directiva)
  }
  RobotMensajero.prototype.mensajear = Milonga.mensajear
  Object.setPrototypeOf(RobotMensajero.prototype, Robot.prototype)
}


// Ejercicio 7
function ejercicio7() {
  Robot.prototype.reprogramar = function(nuevadir,funcnuevadir) {
      delete this[this.directiva]

      if(this.directiva == nuevadir){
        this.directiva = "..."
      }else{
        this.directiva = nuevadir
        this[nuevadir] = funcnuevadir
      }
  }
  // //Queremos que los robots construídos con otra función constructora también puedan usar el método
  // RobotMensajero.prototype.reprogramar = Robot.prototype.reprogramar 

  Robot.prototype.solicitarAyuda = function(nuevoAyudante) {
    if(this.ayudante == undefined){
      if(nuevoAyudante.directiva != this.directiva){
        nuevoAyudante.reprogramar(this.directiva, this[this.directiva])
      }else{
        nuevoAyudante[this.directiva] = this[this.directiva]
      }
      this.ayudante = nuevoAyudante 

    }else {
      (this.ayudante).solicitarAyuda(nuevoAyudante)
    }
  }

  // RobotMensajero.prototype.solicitarAyuda = Robot.prototype.solicitarAyuda
  Malambo.solicitarAyuda = Robot.prototype.solicitarAyuda

}

// Editen esta función para que devuelva lo que quieran mostrar en la salida.
function calcularResultado() {
  let res = "";
  res += "<b>Ejercicio 1</b>\n" + crearTest(1, testEjercicio1)();
  res += "\n";
  res += "<b>Ejercicio 2</b>\n" + crearTest(2, testEjercicio2)();
  res += "\n";
  res += "<b>Ejercicio 2 (agregado por el grupo) </b>\n" + crearTest(2, testEjercicio2grupo)();
  res += "\n";
  res += "<b>Ejercicio 3</b>\n" + crearTest(3, testEjercicio3)();
  res += "\n";
  res += "<b>Ejercicio 4</b>\n" + crearTest(4, testEjercicio4)();
  res += "\n";
  res += "<b>Ejercicio 4 (agregado por el grupo) </b>\n" + crearTest(4, testEjercicio4grupo)();
  res += "\n";
  res += "<b>Ejercicio 5</b>\n" + crearTest(5, testEjercicio5)();
  res += "\n";
  res += "<b>Ejercicio 5 (agregado por el grupo) </b>\n" + crearTest(5, testEjercicio5grupo)();
  res += "\n";
  res += "<b>Ejercicio 6</b>\n" + crearTest(6, testEjercicio6)();
  res += "\n";
  res += "<b>Ejercicio 6 (agregado por el grupo) </b>\n" + crearTest(6, testEjercicio6grupo)();
  res += "\n";
  res += "<b>Ejercicio 7</b>\n" + crearTest(7, testEjercicio7)();
  res += "\n";
  res += "<b>Ejercicio 7 (agregado por el grupo) </b>\n" + crearTest(7, testEjercicio7grupo)();
  return res;
}

// Agreguen aquí los tests representados como funciones que toman un objeto res como argumento.
  // Pueden llamar a res.write para escribir en la salida
  // Pueden llamar a res.test para
    // probar que una condición se cumple (pasándole la condición como único argumento)
    // o para probar que dos valores son iguales (pasándole dos argumentos)

// Test Ejercicio 1
function testEjercicio1(res) {
  res.write("Nombre de Malambo: " + Malambo.nombre);
  res.test(Malambo.nombre, "Malambo");
  res.write("Peso de Malambo: " + Malambo.peso);
  res.test(Malambo.peso, 500);
}

// Test Ejercicio 2
function testEjercicio2(res) {
  res.write("Nombre de Chacarera: " + Chacarera.nombre);
  res.test(Chacarera.nombre, "Chacarera");
  res.write("Peso de Chacarera: " + Chacarera.peso);
  res.test(Chacarera.peso, 1500);
  res.test(!Chacarera.isPrototypeOf(Malambo));
  res.test(Malambo.isPrototypeOf(Chacarera));
}

function testEjercicio2grupo(res){
  //Queremos ver que, modificando el peso de Malambo (que es prototipo de Chacarera), no modificamos el de Chacarera
  res.write("Peso de Malambo: " + Malambo.peso);
  
  let chacareraPeso = Chacarera.peso
  res.write("Peso de Chacarera: " + Chacarera.peso);
  
  res.write("Actualizamos peso de Malambo: ")
  Malambo.peso = 605
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);

  res.assert((Chacarera.peso == chacareraPeso), "El peso de Chacarera cambió")
}

// Test Ejercicio 3
function testEjercicio3(res) {
  res.write("Presentación de Malambo: " + Malambo.presentarse());
  res.test(Malambo.presentarse(), "Hola, soy Malambo y me encanta limpiar.");
  res.write("Presentación de Chacarera: " + Chacarera.presentarse());
  res.test(Chacarera.presentarse(), "Hola, soy Chacarera y me encanta cortar el pasto.");

  res.write("--");
  let peso_malambo = Malambo.peso;
  res.write("Peso de Malambo antes de limpiar: " + peso_malambo);
  res.test(Malambo.limpiar(), "limpiar");
  peso_malambo ++;
  res.write("Peso de Malambo después de limpiar: " + peso_malambo);
  res.test(Malambo.peso, peso_malambo);
}

// Test Ejercicio 4
function testEjercicio4(res) {
  let C = new Robot("C", 100, "..", function(){return null});
  res.test(C.presentarse(), "Hola, soy C y me encanta ...");
  res.test(!Malambo.isPrototypeOf(C));
  res.test(Robot.prototype.isPrototypeOf(C));

  let malambo_sabe_limpiar = "limpiar" in Malambo
  let chacarera_sabe_limpiar = "limpiar" in Chacarera
  let A = nuevoRobot("A", 250, ".");
  let A_sabe_limpiar = "limpiar" in A;
  let B = new Robot("B", 250, ".", function(){return null});
  let B_sabe_limpiar = "limpiar" in B;
  res.write("Malambo" + si_o_no(malambo_sabe_limpiar) + "sabe limpiar.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_limpiar) + "sabe limpiar.");
  res.write("Si creo un nuevo muñeco con la función original, este" + si_o_no(A_sabe_limpiar) + "sabe limpiar.");
  res.write("Si creo un nuevo muñeco con la función constructora, este" + si_o_no(B_sabe_limpiar) + "sabe limpiar.");
  res.test(malambo_sabe_limpiar);
  res.test(chacarera_sabe_limpiar);
  res.test(A_sabe_limpiar);
  res.test(!B_sabe_limpiar);
}

function testEjercicio4grupo(res) {
  //Testeamos que, aunque en Robot la función presentarse() es basada en la de Malambo, modificar presentarse en una instancia de 
  // Robot no modifica la de Malambo ni de otra instancia de Robot
  let D = new Robot("D", 100, "aspirar", function(){return "aspirado"});
  let E = new Robot("E", 6000, "levantar pesas", function(){return "no pain no gain"})
  res.test(D.presentarse(), "Hola, soy D y me encanta aspirar.");
  res.test(E.presentarse(), "Hola, soy E y me encanta levantar pesas.")
  res.test(Malambo.presentarse(), "Hola, soy Malambo y me encanta limpiar.");

  D.presentarse = function(){return "Hola, soy D."}
  res.test(D.presentarse(), "Hola, soy D.");
  let M_presentarse = (Malambo.presentarse() == "Hola, soy Malambo y me encanta limpiar.")
  res.test(M_presentarse)
  res.write("Si creo una instancia con Robot y modifico su función de presentarse, la función de presentarse de Malambo " + si_o_no(!M_presentarse) + "se modifica")
  let E_presentarse = (E.presentarse() == "Hola, soy E y me encanta levantar pesas.")
  res.test(E_presentarse)
  res.write("Si creo una instancia con Robot y modifico su función de presentarse, la función de presentarse de otras instancias " + si_o_no(!E_presentarse) + "se modifican")

}


// Test Ejercicio 5
function testEjercicio5(res) {
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);
  let peso_malambo = Malambo.peso;
  let peso_chacarera = Chacarera.peso;
  let resultado_1 = Milonga.mensajear(Malambo,Chacarera,"limpiar")
  res.write("Resultado Malambo -> limpiar -> Chacarera: " + resultado_1);
  res.test(resultado_1, "limpiar");
  peso_malambo++;
  peso_chacarera++;
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);
  res.test(Malambo.peso, peso_malambo);
  res.test(Chacarera.peso, peso_chacarera);

  res.write("--");
  let A = new Robot("A", 1, "fA", function(){return "fB";})
  let B = new Robot("B", 1, "fB", function(){return "fC";})
  let resultado_2 = Milonga.mensajear(A, B, "fA")
  res.write("Resultado A -> fA -> B: " + resultado_2);
  let resultado_3 = Milonga.mensajear(A, B, "fB")
  res.write("Resultado A -> fB -> B: " + resultado_3);
  let resultado_4 = Milonga.mensajear(B, A, "fA")
  res.write("Resultado B -> fA -> A: " + resultado_4);
  res.test(resultado_2, "fA");
  res.test(resultado_3, "fC");
  res.test(resultado_4, "fC");
}

function testEjercicio5grupo(res) {
  let R = new Robot("Robotito", 10, "bañar perros", function(){return "bañar perros"})
  let malambo_peso = Malambo.peso;
  let resultado = Milonga.mensajear(Malambo, R, "limpiar");
  res.write("Resultado Malambo -> limpiar -> Robotito: " + resultado);
  res.test(resultado, "limpiar");
  let peso_inc = (Malambo.peso == malambo_peso);
  res.test(peso_inc);
  res.write("Como Robotito es una instancia de Robot y no sabe responder a 'limpiar', no le devuelve la directiva a Malambo y el peso de Malambo " + si_o_no(!peso_inc) + "incrementa")
}


// Test Ejercicio 6
function testEjercicio6(res) {
  let C = new RobotMensajero("C", 1000, "..", function(){return null});
  res.test(C.presentarse(), "Hola, soy C y me encanta ...");
  res.test(!Milonga.isPrototypeOf(C));
  res.test(RobotMensajero.prototype.isPrototypeOf(C));

  let peso_malambo = Malambo.peso;
  let peso_chacarera = Chacarera.peso;
  let resultado_1 = C.mensajear(Malambo,Chacarera,"limpiar")
  res.write("Resultado Malambo -> limpiar -> Chacarera: " + resultado_1);
  res.test(resultado_1, "limpiar");
  peso_malambo++;
  peso_chacarera++;
  res.write("Peso de Malambo: " + Malambo.peso);
  res.write("Peso de Chacarera: " + Chacarera.peso);
  res.test(Malambo.peso, peso_malambo);
  res.test(Chacarera.peso, peso_chacarera);
  res.test(C.hasOwnProperty("mensajear"), false);
}

function testEjercicio6grupo(res) {
  //Queremos ver que cuenta con la habilidad de presentarse, aunque no sea un atributo propio (sino porque Robot.prototype es 
  //prototipo de RobotMensajero.prototype, y este de Mensajerito)
  let mensajerito = new RobotMensajero("Mensajerito", 3, "..", function(){return "mensaje"})
  res.test(!mensajerito.hasOwnProperty("presentarse"))
  
  let mensajeritoSePresenta = (mensajerito.presentarse() == "Hola, soy Mensajerito y me encanta ...")
  res.test(mensajeritoSePresenta)

  res.write("Los robots mensajeron" + si_o_no(mensajeritoSePresenta)  + "pueden presentarse aunque no lo tengan como propiedad propia.")
}


// Test Ejercicio 7
function testEjercicio7(res) {
  let A = new Robot("A", 0.2, "a", function(){return "a"});
  let B = new Robot("B", 0.3, "b", function(){return "b"});
  let C = new Robot("C", 0.3, "c", function(){return "c"});

  res.test("a" in A);
  res.write("Directiva de A antes de cambiar su directiva a \"a\": " + A.directiva);
  A.reprogramar("a");
  res.test(A.directiva, "...");
  res.assert(!("a" in A), "A no debería tener el método \"a\" después de reprogramar.");
  res.write("Directiva de A después de cambiar su directiva a \"a\": " + A.directiva);

  res.write("Directiva de A antes de ayudar a B: " + A.directiva);
  res.write("Ayudante de B antes de pedir ayuda a A: " + nombre(B.ayudante));
  res.assert(!("b" in A), "A no debería tener el método \"b\" después de reprogramar.");
  B.solicitarAyuda(A);
  res.assert("b" in A, "A debería tener el método \"b\" después de de que B le pide ayuda.");
  res.test(A.b(), "b");
  res.write("Directiva de A después de ayudar a B: " + A.directiva);
  res.write("Ayudante de B después de pedir ayuda a A: " + nombre(B.ayudante));

  res.write("--");

  res.write("Directiva de C antes de ayudar a B: " + C.directiva);
  res.write("Ayudante de B antes de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A antes de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(!("ayudante" in A));
  res.test(A.directiva, "b");
  B.solicitarAyuda(C);
  res.write("Directiva de C después de ayudar a B: " + C.directiva);
  res.write("Ayudante de B después de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A después de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(A.ayudante, C);
  res.assert(!("ayudante" in C),"C no debería tener ayudante.");
  res.test(A.directiva, "b");
  res.test(C.directiva, "b");

  res.write("--");

  let malambo_sabe_pedir_ayuda = "solicitarAyuda" in Malambo
  let chacarera_sabe_limpiar = "limpiar" in Chacarera
  let chacarera_sabe_pedir_ayuda = "solicitarAyuda" in Chacarera
  let chacarera_sabe_presentarse = "presentarse" in Chacarera
  res.write("Malambo" + si_o_no(malambo_sabe_pedir_ayuda) + "sabe pedir ayuda.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_limpiar) + "sabe limpiar.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_pedir_ayuda) + "sabe pedir ayuda.");
  res.write("Chacarera" + si_o_no(chacarera_sabe_presentarse) + "sabe presentarse.");
  res.test(malambo_sabe_pedir_ayuda);
  res.test(chacarera_sabe_pedir_ayuda);
}

function testEjercicio7grupo(res){
  let A = new Robot("A", 0.2, "a", function(){return "a"});
  let B = new Robot("B", 0.3, "b", function(){return "b"});
  let C = new Robot("C", 0.3, "c", function(){return "c"});
  let D = new Robot("D", 0.6, "d", function(){return "d"});

  res.write("B le pide ayuda a A: ")
  res.write("Directiva de A antes de ayudar a B: " + A.directiva);
  res.write("Ayudante de B antes de pedir ayuda a A: " + nombre(B.ayudante));
  B.solicitarAyuda(A);
  res.assert("b" in A, "A debería tener el método \"b\" después de de que B le pide ayuda.");
  res.test(A.b(), "b");
  res.write("Directiva de A después de ayudar a B: " + A.directiva);
  res.write("Ayudante de B después de pedir ayuda a A: " + nombre(B.ayudante));
  res.write("--")

  res.write("B le pide ayuda a C: ")
  res.write("Directiva de C antes de ayudar a B: " + C.directiva);
  res.write("Ayudante de B antes de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A antes de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(!("ayudante" in A));
  res.test(A.directiva, "b");
  B.solicitarAyuda(C);
  res.write("Directiva de C después de ayudar a B: " + C.directiva);
  res.write("Ayudante de B después de pedir ayuda a C: " + nombre(B.ayudante));
  res.write("Ayudante de A después de que B le pida ayuda a C: " + nombre(A.ayudante));
  res.test(B.ayudante, A);
  res.test(A.ayudante, C);
  res.assert(!("ayudante" in C),"C no debería tener ayudante.");
  res.test(A.directiva, "b");
  res.test(C.directiva, "b");

  // Si B solicita otro ayudante, como su ayudante A ya tiene como ayudante a C, el nuevo ayudante D debería ser ayudante de C
  res.write("--")
  res.write("B le pide ayuda a D: ")
  res.write("Directiva de D antes de ayudar a B: " + D.directiva);
  B.solicitarAyuda(D)
  res.write("Ayudante de B después de pedir ayuda a D: " + nombre(B.ayudante));
  res.test(B.ayudante, A)
  res.write("Ayudante de A después de que B le pida ayuda a D: " + nombre(A.ayudante));
  res.test(A.ayudante, C)
  res.write("Ayudante de C después de que B le pida ayuda a D: " + nombre(C.ayudante));
  res.test(C.ayudante, D)
  res.test(D.directiva, "b");

  //Caso en que tenemos robots lavarropas con misma directiva, pero cuya función de directiva es diferente (caso que no cubrimos en la primer
  //entrega, mencionado por corrector en devolución), y uno le pide ayuda a otro. 
  let lavarropas1 = new Robot("Lavarropas 1", 0.2, "lavarropa", function(){return "a"});
  let lavarropas2 = new Robot("Lavarropas 2", 0.3, "lavarropa", function(){return "b"});
  res.write("Directiva de Lavarropas 1: " + lavarropas1.directiva)
  res.write("Directiva de Lavarropas 2 antes de que Lavarropas 1 le pida ayuda: " + lavarropas2.directiva)
  res.test(lavarropas1.lavarropa() != lavarropas2.lavarropa())
  lavarropas1.solicitarAyuda(lavarropas2)
  res.write("Directiva de Lavarropas 2 luego de que Lavarropas 1 le pida ayuda: " + lavarropas2.directiva)
  res.assert(lavarropas1.lavarropa() == lavarropas2.lavarropa(), "Los dos lavarropas deberían realizar la misma acción")
}


function nombre(objeto) {
  if (objeto) return objeto.nombre;
  return "Ninguno";
}

function si_o_no(bool) {
  return (bool ? " " : " <b>NO</b> ")
}

// Función auxiliar que crea un test genérico a partir de un número i y una función f
function crearTest(i, f) {
  return function() {
    eval("ejercicio"+i)();
    let res = {
      text:"",
      write: function(s) {
        this.text += s + "\n";
      },
      test: function(actual, expected) {
        if (expected !== undefined) {
          if (actual !== expected) {
            fail(i, "Se esperaba " + expected + " pero se obtuvo: " + actual)}
        } else {
          if (actual !== true) {
            fail(i, "Falló la condición del test.")
          }
        }
      },
      assert: function(actual, message) {
        if (actual !== true) {
          fail(i, "Falló la condición del test: " + message);
        }
      }
    };
    try {
      f(res);
    } catch (e) {
      fail(i, e);
    }
    return res.text;
  }
}

let Malambo = undefined
let nuevoRobot = undefined
let Chacarera = undefined
let Robot = undefined
let Milonga = undefined
