package runtime;
import java.lang.Math;
import java.text.DecimalFormat;

public class Runtime
{
    private float[][] results = new float[2][2];

    public void run (BayesProgram p)
    {
        //bayes is O(2^n)
        int nStates = (int)Math.pow(2, p.nSamples);
        int nRepetitions;

        for (int i=0; i<nStates; i++)
        {
//        System.out.println("Run: "+i);
            for (int j=0; j<p.nSamples; j++)
            {
                //calculate whether the variable should be true
                nRepetitions = nStates / (int)Math.pow(2, j);
                p.samples[j] = (i % nRepetitions) / (nRepetitions / 2);
//                System.out.println(p.samples[j]);

            }

            //run the program, store if the run was true
            boolean runTrue = p.run() == 1.0f;
//            System.out.println(p.probability+ " "+runTrue);

            //update the results
            results [p.valid==1.0?0:1] [runTrue?0:1] += p.probability*100;
        }

        printResults();
    }

    private void printResults()
    {
        String[][] words = {{"Valid", "Invalid"}, {"true", "false"}};
        DecimalFormat df = new DecimalFormat("#.##");

        for (int i=0; i<2; i++)
        {
            for (int j=0; j<2; j++)
            {
                System.out.printf("%s, %s: %s%%\n",
                                  words[0][i], words[1][j],
                                  df.format(results[i][j]));
            }
        }
    }
}
