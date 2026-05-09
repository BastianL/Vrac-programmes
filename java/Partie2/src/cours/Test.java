package cours;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;

public class Test {
	public static void main(String[] args){
		LocalDateTime currentTime = LocalDateTime.now();
		System.out.println("Date et heure courante : " + currentTime);
		
		LocalDate date1 = currentTime.toLocalDate();
		System.out.println("Date courante : " + date1);
		
		Month mois = currentTime.getMonth();
		int jour = currentTime.getDayOfMonth();
		int heure = currentTime.getHour();
		
		System.out.println("Mois : " + mois +", jour : " + jour + ", heure : " + heure);
		
		LocalDateTime date2 = currentTime.withDayOfMonth(25).withYear(2023).withMonth(12);
		System.out.println("Date modifiée : " + date2);
		
		LocalDate date3 = LocalDate.of(2023, Month.DECEMBER, 25);
		
		LocalTime parsed = LocalTime.parse("20:15:30");
		System.out.println("Date parsée : " + parsed);
	}
}
