package cours;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class DateTimeManipulation {
	public static void main(String[] args) {
		LocalDateTime ldt = LocalDateTime.parse("2018-01-01T01:33:00");
		List<ZoneId> lzi = Arrays.asList(
				ZoneId.of("Europe/Paris"),
				ZoneId.of("Asia/Tokyo"),
				ZoneId.of("America/Anchorage")
		);
		
		lzi	.stream()
			.forEach((x) -> {
				System.out.println(x + " : \t" + ldt.atZone(x).toInstant());
			});
	}
}
