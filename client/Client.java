package client;

import java.io.IOException;
import java.net.Socket;


public class Client {
	private static final int port = 7000;

	public static void main(String[] args) throws Exception, IOException {
		Socket cli = new Socket(args[0], port);
		ClientInfo info = new ClientInfo();
		Reader reader = new Reader(cli, info);
		Stub stub = new Stub(cli, info);

		reader.start();
		stub.start();
	}
}
