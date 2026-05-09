package cours;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.*;

public class TestFunction {
	public static void main(String[] args) {
		ToDoubleFunction<String> stringToDoubleLambda = (s) -> Double.parseDouble(s);
		ToDoubleFunction<String> stringToDoubleRef = Double::parseDouble;
		System.out.println(stringToDoubleLambda.applyAsDouble("0.1234567"));
		System.out.println(stringToDoubleRef.applyAsDouble("0.1234567"));
		
		Consumer<String> stringPrinterLambda = (s) -> System.out.println(s);
		Consumer<String> stringPrinterRef = System.out::println;
		stringPrinterLambda.accept("Bonjour !");
		stringPrinterRef.accept("Bonjour !");
		
		ToIntFunction<String> testNew = Integer::new;
		Integer i = testNew.applyAsInt("1235");
		System.out.println("New Integer created : " + i.getClass());
	}
}
