package parser.csv;

import java.text.Normalizer;

public class Paragem {
   
    private final int NR_FIELDS = 11;

    private int gid;
    private double latitude;
    private double longitude;
    private String estado_de_conservacao;
    private String tipo_de_abrigo;
    private String abrigo_com_publicidade;
    private String operadora;
    private String carreira;
    private int codigo_de_rua;
    private String nome_da_rua;
    private String freguesia;

    public Paragem () {
        this.emptyParagem();
    }

    public void emptyParagem () {
        
        this.gid = -1;
        this.latitude = 0;
        this.longitude = 0;
        this.estado_de_conservacao = "undefined";
        this.tipo_de_abrigo = "undefined";
        this.abrigo_com_publicidade = "undefined";
        this.operadora = "undefined";
        this.carreira = "undefined";
        this.codigo_de_rua = -1;
        this.nome_da_rua = "undefined";
        this.freguesia = "undefined";
    }

    public void loadFromString(String st, String dataSet) {

        String[] parts = st.split(";");

        if (parts.length < NR_FIELDS) {
            System.out.println("\b\b\b[err] Anomaly parse error ("+dataSet+"): " + st);
            emptyParagem();
            return;
        }

        try {
            this.gid = Integer.parseInt(parts[0]);
            
            if (parts[1].equals("undefined") || parts[2].equals("undefined")) {

                this.latitude = -999999;
                this.longitude = -999999;

            } else {

                this.latitude = getRoundedValue(Double.parseDouble(parts[1]), 4);
                this.longitude = getRoundedValue(Double.parseDouble(parts[2]), 4);
            }

            this.estado_de_conservacao = Normalizer.normalize(parts[3].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
            this.tipo_de_abrigo = Normalizer.normalize(parts[4].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
            this.abrigo_com_publicidade = Normalizer.normalize(parts[5].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
            this.operadora = Normalizer.normalize(parts[6].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
            this.carreira = Normalizer.normalize(parts[7].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
            this.codigo_de_rua = Integer.parseInt(parts[8]);
            this.nome_da_rua = Normalizer.normalize(parts[9].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
            this.freguesia = Normalizer.normalize(parts[10].replace("'", ""), Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "");     

        } catch (Exception e) {

            System.out.println("\b\b\b[err] Line format is bad ("+dataSet+"): " + st);
            emptyParagem();
        }
    }

    public int getGid() {
        return this.gid;
    }

    public String getCarreira() {
        return this.carreira;
    }

    public String toPrologFact() {
        
        StringBuilder sb = new StringBuilder();

        sb.append("paragem(");
        sb.append(gid).append(", ");
        sb.append(latitude).append(", ");
        sb.append(longitude).append(", ");
        sb.append("'").append(estado_de_conservacao).append("'").append(", ");
        sb.append("'").append(tipo_de_abrigo).append("'").append(", ");
        sb.append("'").append(abrigo_com_publicidade).append("'").append(", ");
        sb.append("'").append(operadora).append("'").append(", ");
        //sb.append("'").append(carreira).append("'").append(", ");
        sb.append(codigo_de_rua).append(", ");
        sb.append("'").append(nome_da_rua).append("'").append(", ");
        sb.append("'").append(freguesia).append("'").append(").");
    
        return sb.toString();
    }

    public double getRoundedValue(double value, int precision) {
        
        int scale = (int) Math.pow(10, precision);
     
        return (double) Math.round(value * scale) / scale;
    }
}