package runtime;
/**A class for helping the runtime system communicate with a bayes program*/
public abstract class BayesProgram
{
    public int nSamples;
    public float[] samples; //for holding whether a variable is true or not
    public float valid; //whether the run is valid (1.0=true, 0.0=false)
    public float probability; //probability of the run
    public int varCount;

    public BayesProgram(int nSamples)
    {
        this.nSamples = nSamples;
        samples = new float[nSamples];
    }

    //the main method of a bayes program in the bytecode
    abstract public float run();

    public void initRun()
    {
        probability = 1.0f;
        varCount = -1;
        valid = 1.0f;
    }
}
