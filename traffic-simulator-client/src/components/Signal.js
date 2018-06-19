import React from 'react'
import PropTypes from 'prop-types'
import { OverlayTrigger, Popover } from 'react-bootstrap'
import cx from 'classnames'

export class Signal extends React.Component {
  static propTypes = {
    signal: PropTypes.object.isRequired
  }

  popoverLeft = (
    <Popover id="popover-positioned-left" title="Popover left">
      <strong>Holy guacamole!</strong> Check this info.
    </Popover>  
  )  

  render() {
    return (
      <OverlayTrigger
        trigger={['hover', 'focus']}
        placement="bottom"
        overlay={this.popoverLeft}
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
}

export default Signal