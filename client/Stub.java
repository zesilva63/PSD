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
	
	private String username;

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

		System.out.println("\nConnection ended!");
		System.exit(0);
	}

	private int showMenu() {
		int option = 0;

		try {
			if (!client.isLogged())
				option = menu.show(initialMenu);
			else {
				sessionMenu[0] = "1) Read notifications " + "(" + client.getNumberOfNotifications() + ")";
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
			case 4: subscribeCompany();
					break;
			case 5: sell();
					break;
			case 6: buy();
					break;
			case 7: closeAuction();
					break;
		}
	}

	private static byte[] recvMsg(InputStream inpustream) {
    try {
            byte len[] = new byte[4096];
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
		
		User c = User.newBuilder().setUsername(username).setPassword(password).build();
		Message req = Message.newBuilder().setType("REGISTER").setUser(c).build();
		byte[] result = req.toByteArray();
		os.write(result);

		String response = client.getResponse();
		System.out.println("RESPONSE: " +response);
		if (client.getReplyStatus()) {
			
			req = Message.newBuilder().setType("LOGIN").setUser(c).build();
			result = req.toByteArray();
			os.write(result);

			response = client.getResponse();

			if(client.getReplyStatus()) {
				client.setLogged(true);
			}
		}
		
		menu.printResponse(response);
	}

	private void login() throws IOException {
		String username = menu.readString("Username: ");
		String password = menu.readString("Password: ");
		
		User c = User.newBuilder().setUsername(username).setPassword(password).build();
		Message req = Message.newBuilder().setType("LOGIN").setUser(c).build();
		byte[] result = req.toByteArray();
		os.write(result);
		String response = client.getResponse();

		if(client.getReplyStatus()) {
			client.setLogged(true);
			this.username = username;
		}else {
			menu.printResponse(response);
		}
	}

	private void readNotifications() {
		int amountNotifications;
		String notifications;

		synchronized (client) {
			amountNotifications = client.getNumberOfNotifications();
			notifications = client.getNotifications();
		}

		if (amountNotifications == 0)
			notifications = "> Still no notifications to present!\n";

		menu.printResponse(notifications);
	}

	private void subscribeCompany() {
		String company = menu.readString("Company to subscribe: ");
		Message req = Message.newBuilder().setType("SUBSCRIBE").setDest(comp).build();
		
		byte[] result = m.toByteArray();
		os.write(result);

		String response = client.getResponse();
		menu.printResponse(response);
	}

	private void sell() throws IOException {
		String company = menu.readString("Company: ");
		int quantity = menu.readInt("Quantity to buy: ");
		float price = menu.readFloat("Limit price: ");
		
		User c = User.newBuilder().setUsername(username).build();
		Order o = Order.newBuilder().setCompany(company).setQuantity(quantity).setPrice(price).build();
		Message m = Message.newBuilder().setType("SELL").setUser(c).setOrder(o).build();
		byte[] result = m.toByteArray();

		os.write(result);
		
		String response = client.getResponse();

		menu.printResponse(response);
	}

	private void buy() throws IOException {
		String company = menu.readString("Company: ");
		int quantity = menu.readInt("Quantity to buy: ");
		float price = menu.readFloat("Limit price: ");
		
		User c = User.newBuilder().setUsername(username).build();
		Order o = Order.newBuilder().setCompany(company).setQuantity(quantity).setPrice(price).build();
		Message m = Message.newBuilder().setType("BUY").setUser(c).setOrder(o).build();
		
		byte[] result = m.toByteArray();
		os.write(result);
		
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

		initialMenu[0] = "1) Register";
		initialMenu[1] = "2) Login";

		sessionMenu[1] = "2) Subscribe company";
		sessionMenu[2] = "3) Sell shares";
		sessionMenu[3] = "4) Buy shares";
		sessionMenu[4] = "5) Terminar leilão";
	}
}
