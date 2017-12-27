package client;

import client.Protocol.*;

class Printer {

  static void print(AuthenticationResponse p) {
    System.out.println("Descrição: " + p.getDescription());
  }

}

