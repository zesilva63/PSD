package client;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.NoSuchElementException;


import client.Protocol.*;

public class Stub extends Thread {
	private Socket cliSocket;
	private ClientInfo client;
	private PrintWriter out;
	private InputStream is;
    private OutputStream os;
	private Menu menu;
	private String[] initialMenu;
	private String[] sessionMenu;

	Stub(Socket cliSocket, ClientInfo client) throws IOException {
		this.cliSocket = cliSocket;
		this.client = client;
		this.is = cliSocket.getInputStream();
		this.os = cliSocket.getOutputStream();
		out = new PrintWriter(os, true);
		menu = new Menu();
		setUpMenus();
	}

	public void run() {
		int option;

		while((option = showMenu()) != -1) {
			client.setCommand(option);
			try {
				runCommand(option);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		System.out.println("\nLigação terminada!");
		System.exit(0);
	}

	private int showMenu() {
		int option = 0;

		try {
			if (!client.isLogged())
				option = menu.show(initialMenu);
			else {
				sessionMenu[0] = "1) Ler notificações " + "(" + client.getNumberOfNotifications() + ")";
				option = menu.show(sessionMenu) + 2;
			}
		} catch (NoSuchElementException e) {
			return -1;
		}

		return option;
	}

	private void runCommand(int option) throws IOException {
		switch(option) {
			case 1: signup();
					break;
			case 2: login();
					break;
			case 3: readNotifications();
					break;
			case 4: listAuctions();
					break;
			case 5: startAuction();
					break;
			case 6: bid();
					break;
			case 7: closeAuction();
					break;
		}
	}

	public static byte[] recvMsg(InputStream inpustream) {
        try {

            byte len[] = new byte[1024];
            int count = inpustream.read(len); 
            byte[] temp = new byte[count];
            for (int i = 0; i < count; i++) { 
                temp[i] = len[i]; 
            } 
            return temp;
        } catch (Exception e) {
            System.out.println("recvMsg() occur exception!" + e.toString());
        }
        return null;
}

	private void signup() throws IOException {
		String username = menu.readString("Username: ");
		String password = menu.readString("Password: ");
		
		ClientData c = ClientData.newBuilder().setUsername(username).setPassword(password).build();
		Message req = Message.newBuilder().setType("REGISTER").setClientData(c).build();
		byte[] result = req.toByteArray();
		os.write(result);
		

		byte[] msg = recvMsg(is);
		Message rep = Message.parseFrom(msg);
		
		if (rep.getResponse().getResult().equals("OK")) {
		//	req = AuthenticationRequest.newBuilder().setUsername(username).setPassword(password).setRequest(AuthenticationRequest.RequestType.LOGIN).build();
		//	req.writeTo(os);
		//	rep = AuthenticationResponse.parseDelimitedFrom(is);
			client.setLogged(true);
		}

		menu.printResponse(rep.getResponse().getDescription());
		
	}

	private void login() {
		String username = menu.readString("Username: ");
		String password = menu.readString("Password: ");
		String query = String.join(" ", "LOGIN", username, password);

		out.println(query);
		String response = client.getResponse();

		if (client.getReplyStatus())
			client.setLogged(true);

		/*
		AuthenticationRequest req = AuthenticationRequest.newBuilder().setUsername(username).setPassword(password).setRequest(AuthenticationRequest.RequestType.LOGIN).build();
		req.writeDelimitedTo(os);
		
		AuthenticationReply rep = AuthenticationReply.parseDelimitedFrom(is);
		
		if (rep.getResponse().equals(AuthenticationReply.ResponseType.OK)) {
			client.setLogged(true);	
		}
		*/
		menu.printResponse(response);



	}

	private void readNotifications() {
		int amountNotifications;
		String notifications;

		synchronized (client) {
			amountNotifications = client.getNumberOfNotifications();
			notifications = client.getNotifications();
		}

		if (amountNotifications == 0)
			notifications = "> Não há novas notificações!\n";
		else
			out.println("CONFIRMAR " + amountNotifications);

		menu.printResponse(notifications);
	}

	private void listAuctions() {
		out.println("LISTAR");

		String response = client.getResponse();
		menu.printResponse(response);
	}

	private void startAuction() {
		String description = menu.readString("Descrição: ");
		String query = String.join(" ", "INICIAR", description);

		out.println(query);
		String response = client.getResponse();

		menu.printResponse(response);
	}

	private void bid() {
		int itemID = menu.readInt("Item ID: ");
		float value = menu.readFloat("Valor: ");
		String query = "LICITAR " + itemID + " " + value;

		out.println(query);
		String response = client.getResponse();

		menu.printResponse(response);
	}

	private void closeAuction() {
		int itemID = menu.readInt("Item ID: ");
		String query = "TERMINAR " + itemID;

		out.println(query);
		String response = client.getResponse();

		menu.printResponse(response);
	}

	private void setUpMenus() {
		initialMenu = new String[2];
		sessionMenu = new String[5];

		initialMenu[0] = "1) Registar";
		initialMenu[1] = "2) Iniciar Sessão";

		sessionMenu[1] = "2) Listar leilões";
		sessionMenu[2] = "3) Iniciar leilão";
		sessionMenu[3] = "4) Licitar item";
		sessionMenu[4] = "5) Terminar leilão";
	}
}
