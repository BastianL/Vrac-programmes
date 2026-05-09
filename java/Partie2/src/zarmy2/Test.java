package zarmy2;

import com.sdz.comportement.*;

public class Test {
	
	public static void main(String[] args) {
		Personnage pers = new Guerrier();
		pers.soigner();
		pers.setSoin(new Operation());
		pers.soigner();
		
		pers.setSoin(new Soin() {
			public void soigner() {
				System.out.println("je soigne avec une classe annonyme ! ");
			}
		});
		
		pers.soigner();
	}
}
