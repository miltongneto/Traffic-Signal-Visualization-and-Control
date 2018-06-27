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

    if(!signal) return this.renderLoading()

    return (
      <div>
        <h3>Sinal de Trânsito</h3>
        {this.renderTable(signal)}
        {this.renderForm(signal)}
      </div>
    )
  }

  renderLoading() {
    return (
      <div>
        loading...
      </div>
    )
  }

  renderForm = (signal) => {
    return (
      <form 
        url={`${process.env.REACT_APP_API_URL}traffic-signal/${signal.trafficId}`} 
        method="POST"
        onSubmit={this.handleSubmit}
      >
        <FieldGroup
          type="text"
          label="Funcionamento"
          name="funcionamento"
          value={signal.funcionamento}
          onChange={(event) => this.handleChange('funcionamento', event.target.value)}
        />

        <FieldGroup
          type="text"
          label="Localização 1"
          name="localizacao1"
          value={signal.localizacao1}
          onChange={(event) => this.handleChange('localizacao1', event.target.value)}
        />

        <FieldGroup
          type="text"
          label="Localização 2"
          name="localizacao2"
          value={signal.localizacao2}
          onChange={(event) => this.handleChange('localizacao2', event.target.value)}
        />

        <FormGroup>
          <ControlLabel>Sinal Sonoro</ControlLabel>
          <FormControl 
            name="sinalSonoro" 
            componentClass="select" 
            value={signal.sinalSonoro}
            onChange={(event) => this.handleChange('sinalSonoro', event.target.value)}
          >
            <option value="S">Sim</option>
            <option value="N">Não</option>
          </FormControl>
        </FormGroup>

        <FormGroup>
          <ControlLabel>Sinalizador Ciclista</ControlLabel>
          <FormControl 
            name="sinalizadorCiclista" 
            componentClass="select" 
            value={signal.sinalizadorCiclista}
            onChange={(event) => this.handleChange('sinalizadorCiclista', event.target.value)}
          >
            <option value="S">Sim</option>
            <option value="N">Não</option>
          </FormControl>
        </FormGroup>

        <FieldGroup
          type="text"
          label="Utilização"
          name="utilizacao"
          value={signal.utilizacao}
          onChange={(event) => this.handleChange('utilizacao', event.target.value)}
        />

        <FieldGroup
          type="number"
          label="Tempo para abrir"
          name="timeToOpen"
          value={signal.timeToOpen}
          onChange={(event) => this.handleChange('timeToOpen', event.target.value)}
        />

        <FieldGroup
          type="number"
          label="Tempo para fechar"
          name="timeToClose"
          value={signal.timeToClose}
          onChange={(event) => this.handleChange('timeToClose', event.target.value)}
        />

        <FormControl type="hidden" name="id" defaultValue={signal.trafficId} />
        <FormControl type="hidden" name="latitude" defaultValue={signal.latitude} />
        <FormControl type="hidden" name="longitude" defaultValue={signal.longitude} />
        <FormControl type="hidden" name="status" defaultValue={signal.status} />
        <FormControl type="hidden" name="lastUpdate" defaultValue={signal.lastUpdate} />
        <Button type="submit">Submit</Button>
        <Button onClick={this.handleChangeStatus} type="button">Mudar status</Button>
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
            <td>Status</td>
            <td>{this.getStatusText(signal.status)}</td>
          </tr>
        </tbody>
      </Table>
    )
  }

  handleSubmit = (event) => {
    axios.post(`${process.env.REACT_APP_API_URL}traffic-signal/${this.props.signalId}`, this.state.signal)
    event.preventDefault() 
  }

  handleChangeStatus = () => {
    axios.post(`${process.env.REACT_APP_API_URL}traffic-signal/forceStatusChange/${this.props.signalId}`)
  }

  handleChange = (key, value) => {
    this.setState((prevState) => {
      prevState.signal[key] = value
      return { signal: prevState.signal }
    })
  }

  getSignalById = async (id) => {
    const { data } = await axios.get(`${process.env.REACT_APP_API_URL}traffic-signal/${id}`)
    
    this.setState({
      signal: data
    })
  }

  getBooleanText = (booleanText) => {
    switch(booleanText.toLowerCase()) {
      case 'n':
        return 'Não'
      case 's':
        return 'Sim'
      default:
        return 'Erro'
    }
  }
 
  getStatusText = (status) => {
    switch(status) {
      case 0:
        return 'Verde'
      case 1:
        return 'Amarelo'
      case 2:
        return 'Vermelho'
      default:
        return 'Erro'
    }
  }
}

export default ViewBar