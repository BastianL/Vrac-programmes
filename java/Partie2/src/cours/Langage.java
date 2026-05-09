package cours;

public enum Langage {
	JAVA ("Langage JAVA", "Eclipse"),
	C ("Langage C", "Code Block"),
	CPlus ("Langage C++", "Visual studio"),
	PHP ("Langage PHP", "PS Pad");
	private String name = "";
	private String editor = "";
	
	Langage(String name, String editor){
		this.name = name;
		this.editor = editor;
	}
	
	public void getEditor(){
		System.out.println("Editor : " + editor);
	}
	
	public String toString() {
		return name;
	}
	
	public static void main(String args[]) {
		Langage l1 = Langage.JAVA;
		Langage l2 = Langage.PHP;
		
		l1.getEditor();
		l2.getEditor();
	}
}
