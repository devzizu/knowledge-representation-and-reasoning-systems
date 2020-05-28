
package parser;

import java.io.File;

import parser.csv.CSVReader;

public class Main {

    public static void main(String[] args) {
    
        System.out.println("> Parser started...");

        File dataSetInputFile = new File(args[0]);

        if (!dataSetInputFile.exists()) {
            System.out.println("\t[error] Could not load " + args[0] + "file."); 
            return;
        }

        System.out.println("> Input file status: " + (dataSetInputFile.canRead()?"Ok.":"Can't read."));
    
        try {

            File dataSetOutputFile = new File(args[1]);
            dataSetOutputFile.createNewFile();

            CSVReader.csvToProlog(dataSetInputFile, dataSetOutputFile);
            
            System.out.println("> Finished. Output file location: " + args[1]);

        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("[error] Got exception while reading file. Try again later.");
        }
    
    }
}