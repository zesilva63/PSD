package exchange;

import java.util.HashMap;
import java.util.Map;

import exchange.Protocol.*;

public class Exchange {

	private Map<String,Company> companies;

	public Exchange() {
		this.companies = new HashMap<>();
	}

	public addRequest(Message m) {
		String comp = m.getRequest().getCompany();
		String user = m.getUser().getUsername();
		int quant = m.getRequest().getQuantity();
		float price = m.getRequest().getPrice();
		Company c = companies.get(comp);
		
		Order o = new Order(user,comp,quant,price);
		c.addOrder(o);
	}

}