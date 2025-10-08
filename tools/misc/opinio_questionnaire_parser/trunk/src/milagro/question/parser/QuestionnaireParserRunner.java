package milagro.question.parser;

import java.io.*;

public class QuestionnaireParserRunner {
	
    public static void main(String[] args) {
	QuestionnaireParser parser = new QuestionnaireParser();
	try {
	    parser.parseRawReportFiles();
	} catch (IOException e) {
	    System.out.println(e.getMessage());
	    e.printStackTrace();
	    System.exit(1);
	}
    }
}
