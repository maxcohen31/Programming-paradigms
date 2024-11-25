public class ContoCorrente {
    public double saldo;

    // costruttore 
    public ContoCorrente(double saldoIniziale) {
        saldo = saldoIniziale;
    }

    // metodi
    public void versa(double somma) {
        saldo += somma;
    }
    public void preleva(double somma) {
        saldo -= somma;
    }

    
}
