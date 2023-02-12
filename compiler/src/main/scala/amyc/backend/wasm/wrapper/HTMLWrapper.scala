package amyc.backend.wasm.wrapper

import amyc.backend.wasm.Module

object HTMLWrapper {

  def apply(moduleFile: String, module: Module): String =
    s"""|<!doctype html>
        |
        |<html>
        |  <head>
        |    <meta charset="utf-8">
        |    <title>${module.name}</title>
        |  </head>
        |
        |  <body>
        |    <p id="htmlText"></p>
        |    <script>
        |
        |      // This library function fetches the wasm module at 'url', instantiates it with
        |      // the given 'importObject', and returns the instantiated object instance
        |      // Taken from https://github.com/WebAssembly/spec
        |      function fetchAndInstantiate(url, importObject) {
        |        return fetch(url).then(response =>
        |          response.arrayBuffer()
        |        ).then(bytes =>
        |          WebAssembly.instantiate(bytes, importObject)
        |        ).then(results =>
        |          results.instance
        |        );
        |      }
        |
        |      const log = (line) => document.getElementById("htmlText").innerHTML += line + "<br>";
        |      const waitInput = () => window.prompt("Input to WASM program:");
        |      const exit = () => log('<span style="color: red">Exited</span>');
        |
        |      const memory = new WebAssembly.Memory({initial:100});
        |
        |      $importObject
        |
        |      fetchAndInstantiate('$moduleFile', importObject).then(function(instance) {
        |""".stripMargin ++
      module.functions.filter(_.result.isEmpty).map { f =>
        s"        instance.exports.${f.name}();\n"
      }.mkString ++
      """|      });
         |    </script>
         |  </body>
         |
         |</html>
         |
          """.stripMargin

  /** JavaScript object containing the values to be imported into the WASM instance.
    * Requires a waitInput(), log(line) and exit() function to be in scope.
    */
  lazy val importObject: String =
    s"""const importObject = {
       |  system: {
       |    mem: memory,
       |
       |    printInt: function(arg) {
       |      log(arg);
       |      0;
       |    },
       |
       |    printString: function(arg) {
       |      var bufView = new Uint8Array(memory.buffer);
       |      var i = arg;
       |      var result = "";
       |      while(bufView[i] != 0) {
       |       result += String.fromCharCode(bufView[i]);
       |       i = i + 1;
       |      }
       |      log(result);
       |      0;
       |    },
       |
       |    readInt: function() {
       |      var res = parseInt(waitInput());
       |      if (isNaN(res)) {
       |        log("Error: Could not parse int");
       |        exit();
       |      } else {
       |        return res;
       |      }
       |    },
       |
       |    // This function has a weird signature due to restrictions of the current WASM format:
       |    // It takes as argument the position that it needs to store the string to,
       |    // and returns the first position after the new string.
       |    readString0: function(memB) {
       |      var s = waitInput();
       |      var size = s.length;
       |      var padding = 4 - size % 4;
       |      var fullString = s + "\u0000".repeat(padding);
       |      var newMemB = memB + size + padding;
       |      var bufView8 = new Uint8Array(memory.buffer);
       |      for (var i = 0; i < fullString.length; i++) {
       |        bufView8[memB + i] = fullString.charCodeAt(i);
       |      }
       |      return newMemB;
       |    }
       |  }
       |};
       |""".stripMargin

}
