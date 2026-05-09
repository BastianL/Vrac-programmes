
public class main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		int j = 20, i = 0;
		try {
			System.out.println(" =>" + (1/0));
		} catch (ClassCastException e) {
			e.printStackTrace();
		}
		finally {
			System.out.println("coucou toi !");
		}		
	}
}
