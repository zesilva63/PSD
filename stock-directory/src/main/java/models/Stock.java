package models;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.base.Optional;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class Stock {
    private Optional<Integer> minimum, maximum, open, close;

    @JsonIgnore
    private Optional<Integer> current;

    public Stock(Optional<Integer> initial) {
        open = initial;
        minimum = initial;
        maximum = initial;
        current = initial;

        close = Optional.absent();
    }

    public Integer getMinimum() {
        return minimum.orNull();
    }

    public Integer getMaximum() {
        return maximum.orNull();
    }

    public Integer getOpen() {
        return open.orNull();
    }

    public Integer getClose() {
        return close.orNull();
    }

    public void apply(Transaction tr) {
        int price = tr.getPrice();

        if (tr.getPrice() > maximum.or(Integer.MIN_VALUE))
            maximum = Optional.of(price);

        if (tr.getPrice() < minimum.or(Integer.MAX_VALUE))
            minimum = Optional.of(price);

        current = Optional.of(price);
    }

    public Stock close() {
        close = Optional.fromNullable(current.orNull());
        return new Stock(close);
    }

    public boolean hasTransactions() {
        return current.isPresent() && open.isPresent();
    }
}