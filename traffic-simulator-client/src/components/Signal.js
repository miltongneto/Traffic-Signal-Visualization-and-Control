import React from 'react'
import PropTypes from 'prop-types'
import { OverlayTrigger, Popover } from 'react-bootstrap'
import cx from 'classnames'

export class Signal extends React.Component {
  static propTypes = {
    signal: PropTypes.object.isRequired
  }

  popover = (signal) => {
    return (
      <Popover id="popover-positioned-down" title="Sinal">
        <table>
          <tbody>
            <tr>
              <td>Utilizacao</td>
              <td>{signal.utilizacao}</td>
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
              <td>{this.getBooleanText(signal.sinalSonoro)}</td>
            </tr>
            <tr>
              <td>Sinalizador Ciclista</td>
              <td>{this.getBooleanText(signal.sinalizadorCiclista)}</td>
            </tr>
          </tbody>
        </table>
      </Popover> 
    )
  }  

  render() {
    return (
      <OverlayTrigger
        trigger={['hover', 'focus']}
        placement="bottom"
        overlay={this.popover(this.props.signal)}
      >
        <div className={this.getClassName(this.props.signal.status)}>
        </div>
      </OverlayTrigger>
    )
  }

  getClassName = (status) => {
    return cx({
      'signal': true, 
      'signal-green': status === 0,
      'signal-yellow': status === 1,
      'signal-red': status === 2
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
}

export default Signal