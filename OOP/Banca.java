public class Banca {
    public static void main(String[] args) {
       // crea conto di 1000 euro
       ContoCorrente c1 = new ContoCorrente(1000);
       ContoCorrente c2 = new ContoCorrente(200);

       // controllo se ci sono più di 700 euro
       if (c1.saldo >= 700) {
        c1.preleva(700);
        c2.versa(700);
       }
       System.out.println("Saldo primo conto: " + c1.saldo);
       System.out.println("Saldo secondo conto: " + c2.saldo);
    }

}