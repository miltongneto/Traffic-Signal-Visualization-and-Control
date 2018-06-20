import React from 'react'
import PropTypes from 'prop-types'
import { FormGroup, ControlLabel, HelpBlock, FormControl, Table, Button } from 'react-bootstrap'
import axios from 'axios'

function FieldGroup({ id, label, help, ...props }) {
  return (
    <FormGroup controlId={id}>
      <ControlLabel>{label}</ControlLabel>
      <FormControl {...props} />
      {help && <HelpBlock>{help}</HelpBlock>}
    </FormGroup>
  )
}

export class ViewBar extends React.Component {
  static props = {
    signalId: PropTypes.number.isRequired
  }

  state = {
    signal: null
  }

  componentDidMount() {
    this.getSignalById(this.props.signalId)
  }

  componentDidUpdate(prevProps) {
    if(prevProps.signalId === this.props.signalId) return

    this.getSignalById(this.props.signalId)
  }

  render() {
    const { signal } = this.state

    if(!signal) return <div></div>

    return (
      <div>
        <h3>Sinal de Trânsito</h3>
        {this.renderTable(signal)}
        {this.renderForm(signal)}
      </div>
    )
  }

  renderForm = (signal) => {
    return (
      <form url={`${process.env.REACT_APP_API_URL}traffic-signal/${signal.trafficId}`}>
        <FieldGroup
          id="formControlsText"
          type="number"
          label="Tempo para abrir"
          name="timeToOpen"
          value={signal.timeToOpen}
        />

        <FieldGroup
          id="formControlsEmail"
          type="number"
          label="Tempo para fechar"
          name="timeToClose"
          value={signal.timeToClose}
        />

        <Button type="submit">Submit</Button>
      </form>
    )
  }

  renderTable = (signal) => {
    return (
      <Table striped hover>
        <thead>
          <tr>
            <th>Coluna</th>
            <th>Valor</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Latitude</td>
            <td>{signal.latitude}</td>
          </tr>
          <tr>
            <td>Longitude</td>
            <td>{signal.longitude}</td>
          </tr>
          <tr>
            <td>Funcionamento</td>
            <td>{signal.funcionamento}</td>
          </tr>
          <tr>
            <td>Localização 1</td>
            <td>{signal.localizacao1}</td>
          </tr>
          <tr>
            <td>Localização 2</td>
            <td>{signal.localizacao2}</td>
          </tr>
          <tr>
            <td>Sinal Sonoro</td>
            <td>{signal.sinalSonoro}</td>
          </tr>
          <tr>
            <td>Sinalizador Ciclista</td>
            <td>{signal.sinalizadorCiclista}</td>
          </tr>
          <tr>
            <td>Utilização</td>
            <td>{signal.utilizacao}</td>
          </tr>
          <tr>
            <td>Status</td>
            <td>{signal.status}</td>
          </tr>
        </tbody>
      </Table>
    )
  }

  getSignalById = async (id) => {
    const { data } = await axios.get(`${process.env.REACT_APP_API_URL}traffic-signal/${id}`)
    
    this.setState({
      signal: data
    })
  }
}

export default ViewBar