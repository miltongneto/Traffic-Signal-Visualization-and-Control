import React, { Component } from 'react'
import logo from './logo.svg'
import { Grid, Row, Col } from 'react-bootstrap'

import { Map, ViewBar } from './components'


class App extends Component {
  state = {
    selectedSignalId: null
  }

  render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Visualização e Controle de Sinal de Tráfego</h1>
          <img src={logo} className="App-logo" alt="logo" />
        </header>
        <Grid style={{ marginTop: '30px' }}>
          <Row>
            <Col xs={12} md={8}>
              <Map onShowSignal={this.onShowSignal} />
            </Col>
            <Col xs={6} md={4}>
              {this.renderViewBarSignal()}
            </Col>
          </Row>
        </Grid>
      </div>
    );
  }

  renderViewBarSignal = () => {
    if(!this.state.selectedSignalId) {
      return this.renderNoSignal()
    }
    
    return <ViewBar signalId={this.state.selectedSignalId} />
  }

  renderNoSignal() {
    return (
      <div>
        <h1>Nenhum sinal selecionado</h1>
      </div>
    )
  }

  onShowSignal = (id) => {
    this.setState({
      selectedSignalId: id
    })
  }
}

export default App
