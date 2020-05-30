
package parser;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collectors;

import parser.csv.CSVReader;

public class Main {

    public static void main(String[] args) {
    
        System.out.println("> Parser started...");

        File dataSetsFolder = new File(args[0]);

        if (!dataSetsFolder.exists()) {
            System.out.println("\t[error] Could not load " + args[0] + " folder."); 
            return;
        }

        System.out.println("> Input folder status (" + dataSetsFolder.list().length + " files): " + (dataSetsFolder.canRead()?"Ok.":"Can't open/read.") + "\n");
    
        try {

            // Input csv files list ordered

            TreeMap<Integer, String> dataSetsOrderedFiles = new TreeMap<>();
            int fileNumber = 0, istart = 0, iend = 0;

            for (String fileName: dataSetsFolder.list()) {

                istart = fileName.indexOf("_");
                iend = fileName.indexOf(".");
                fileNumber = Integer.parseInt(fileName.substring(istart+1, iend));
                
                dataSetsOrderedFiles.put(fileNumber, fileName);
            }
            
            List<File> inputFiles = new ArrayList<>();
            for (String fileOrderName: dataSetsOrderedFiles.values().stream().collect(Collectors.toList())) {
                inputFiles.add(new File(args[0] + fileOrderName));
            }

            // Output prolog file

            File dataSetOutputFile = new File(args[1]);
            dataSetOutputFile.createNewFile();
            
            CSVReader.csvToProlog(inputFiles, dataSetOutputFile);
            
            System.out.println("\n> Finished. Output file location: " + args[1]);

        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("[error] Got exception while reading file. Try again later.");
        }
    
    }
}