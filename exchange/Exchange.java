package exchange;

import java.util.HashMap;
import java.util.Map;
import org.zeromq.ZMQ;
import java.util.List;
import java.util.ArrayList;

import exchange.Protocol.*;

public class Exchange {

	private Map<String, Company> companies;
	private ZMQ.Socket push;
	private ZMQ.Socket pull;
	private ZMQ.Socket pub;

	public static void main(String[] args) {

		ZMQ.Context context = ZMQ.context(1);
		ZMQ.Socket push = context.socket(ZMQ.PUSH);
		push.connect("tcp://localhost:" + args[0]);

		ZMQ.Socket pull = context.socket(ZMQ.PULL);
		pull.connect("tcp://localhost:" + args[1]);

		ZMQ.Socket pub = context.socket(ZMQ.PUB);
		pub.connect("tcp://localhost:" + args[2]);

		Exchange exchange = new Exchange(push, pull, pub);
		DirectoryManager directory = new DirectoryManager();
		MessageCreator creator = new MessageCreator();
		List<Transaction> transactions;

		while (true) {

			Message msg = exchange.recvMessage();

			Order order = exchange.createOrder(msg);
			String company = msg.getOrder().getCompany();

			if (msg.getType().equals("SELL"))
				transactions = exchange.addSellOrder(company, order);
			else
				transactions = exchange.addBuyOrder(company, order);

			for (Transaction t : transactions) {
				if(t != null) {
					try {
						directory.sendTransaction(t);
					} catch (Exception e) {
						System.out.println("Error sending data to Directory");
					}
					// enviar a cada um dos envolvidos
					Message mSeller = creator.createSellerResponse(t);
					Message mBuyer = creator.createBuyerResponse(t);

					push.send(mSeller.toByteArray());
					push.send(mBuyer.toByteArray());
				}
			}
		}
	}



	public Exchange(ZMQ.Socket push , ZMQ.Socket pull, ZMQ.Socket pub) {
		this.companies = new HashMap<>();
		this.push = push;
		this.pull = pull;
		this.pub = pub;
	}

	public List<Transaction> addBuyOrder(String company, Order buyOrder) {
		return this.companies.get(company).registBuyOrder(buyOrder);
	}

	public List<Transaction> addSellOrder(String company, Order sellOrder) {
		return this.companies.get(company).registSellOrder(sellOrder);
	}

	public Order createOrder(Message m) {
		String user = m.getUser().getUsername();
		String company = m.getOrder().getCompany();
		int quantity = m.getOrder().getQuantity();
		float price = m.getOrder().getPrice();

		return new Order(user,company,quantity,price);
	}

	public Message recvMessage() {
		try {
			byte binary[] = pull.recv();
			Message msg = Message.parseFrom(binary);
			return msg;
		} catch (Exception e) {
			System.out.println("recvMsg() occur exception!" + e.toString());
		}
		return null;
	}



}
