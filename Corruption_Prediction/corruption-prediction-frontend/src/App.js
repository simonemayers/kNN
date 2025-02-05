import React, { useState } from 'react';
import axios from 'axios';

function App() {
  const [inputs, setInputs] = useState({});
  const [prediction, setPrediction] = useState("");

  const handleChange = (event) => {
    setInputs(prev => ({ ...prev, [event.target.name]: event.target.value }));
  };

  const handleSubmit = async (event) => {
    event.preventDefault();
    try {
      const response = await axios.post('http://localhost:5000/predict', inputs);
      setPrediction(response.data.prediction);
    } catch (error) {
      console.error('Error fetching prediction:', error);
    }
  };

  return (
    <div className="App">
      <h1>Corruption Prediction</h1>
      <form onSubmit={handleSubmit}>
        <div>
          <label>Variable 1:</label>
          <input type="text" name="var1" value={inputs.var1 || ""} onChange={handleChange} />
        </div>
        <div>
          <label>Variable 2:</label>
          <input type="text" name="var2" value={inputs.var2 || ""} onChange={handleChange} />
        </div>
        {/* Add more inputs as necessary */}
        <button type="submit">Predict</button>
      </form>
      {prediction && <p>Prediction Result: {prediction}</p>}
    </div>
  );
}

export default App;
