package exchange;

import java.util.*;
import exchange.Protocol.*;

public class Company {

    private String company;
	private Queue<Order> buyOrders;
	private Queue<Order> sellOrders;
	private List<Transaction> transactions;

	public Company() {
		this.buyOrders = new PriorityQueue<>(compareByPrice);
		this.sellOrders = new PriorityQueue<>(compareByPrice);
		this.transactions = new ArrayList<>();
	}

	public Transaction registBuyOrder(Order buyOrder) {
	    Order bestSellOrder = sellOrders.poll();
		return newTransaction(bestSellOrder, buyOrder);
	}

	public Transaction registSellOrder(Order sellOrder) {
		Order bestBuyOrder = buyOrders.poll();
		return newTransaction(sellOrder, bestBuyOrder);
	}

	private Transaction newTransaction(Order sellorder, Order buyOrder) {
	    if (isTransactionPossible(sellOrder, buyOrder)) {

	        int quantity = Math.min(sellOrder.getQuantity(), buyOrder.getQuantity());

			float sellPrice = sellOrder.getPrice();
			float buyPrice = buyOrder.getPrice();
			float price = roundPrice((sellPrice + buyPrice) / 2);

	        Transaction transaction = new Transaction(company, sellOrders.getUser(), buyOrder.getUser(), quantity, price);
			transactions.add(transaction);

			if (quantity < buyOrder.getQuantity()) {
				buyOrder.decreaseQuantity(quantity);
				buyOrders.add(buyOrder);
			} else if (quantity > buyOrder.getQuantity()) {
				sellOrder.decreaseQuantity(quantity);	
				sellOrders.add(sellOrder);
			}

			return transaction;
        } else {
			sellOrders.add(sellOrder);
			buyOrders.add(buyOrder);
		}

		return null;
	}

	private boolean isTransactionPossible(Order sellOrder, Order buyOrder) {
		return sellOrder != null and buyOrder != null and 
			   sellOrder.getPrice() <= buyOrder.getPrice();
	}

	private float roundPrice(float price) {
		float rp = price * 100;
		long tmp = Math.round(rp);
		return (float) tmp / 100;
	}

	private Comparator<Order> compareByPrice = (Order o1, Order o2) -> Float.compare(o1.getPrice(), o2.getPrice());
}
