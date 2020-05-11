
package parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

public class Main {

    private static final String CSV_FILE = "paragem_autocarros_oeiras_encoded.csv";
    private static final String CSV_PATH = "../documents/datasets-raw/encoded/";

    private static final String CSV_SRC = CSV_PATH + CSV_FILE;

    private static void parseDataSet (File dataSet) throws Exception {

        BufferedReader br = new BufferedReader(new FileReader(dataSet));

        int i = 0;
        
        String line = br.readLine();
        
        System.out.println("COLUMNS:");
        System.out.println(line);
        
        while ((line = br.readLine()) != null) {
        
            System.out.println(line); 
            i++;
            if (i > 1) return;
        } 

        br.close();
    }

    public static void main(String[] args) {
    
        //-------------------------------------------------------------------------

        System.out.println("Parser running...\n");

        //-------------------------------------------------------------------------

        File dataSet = new File(CSV_SRC);
        //File does not exist
        if (!dataSet.exists()) {
            
            System.out.println("[err] source dataset was not found in: " + CSV_PATH);
            System.out.println("-> Dataset name should be: " + CSV_FILE);
            return; //exit
        }

         //-------------------------------------------------------------------------

        System.out.println("[parser] source dataset found: " + dataSet.getName());

        //tries to parse the file
        try {

            parseDataSet(dataSet);

        } catch (Exception e) {
            
            System.out.println("[err] parse failed");
            System.out.println(e.getMessage());
        }
    
    }
}