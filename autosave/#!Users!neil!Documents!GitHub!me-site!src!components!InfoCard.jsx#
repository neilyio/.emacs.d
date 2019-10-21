import React from "react";
import styled from "styled-components";
import { SmallHeading, BoldBody, SmallNormalBody, SmallBoldBody } from "../typography";

const InfoCardContainer = styled.div.attrs((props) => ({
   background: props.background || 'white'
}))`

    display: flex;
    flex-direction: column;
    align-items: center;
    background: ${props => props.background};
    height: 32em;
    padding: 3em;
    box-sizing: border-box;
    text-align: center;
`;

const BaseImage = styled.img`
    position: relative;
    height: 100%;
`;

const InfoCardHeading = styled(SmallHeading)`
    margin: 0;
    margin-bottom: 0.5em;    
`;

const InfoCardSubHeading = styled(SmallBoldBody)`
    margin: 0;
`;

const InfoCardBody = styled(SmallNormalBody)`
    margin: 0;
`;

const InfoCardDivider = styled.div`
    background-color: #E6E6E6;
    margin: 16px;
    width: 55px;
    height: 1px;
`;

const InfoCard = ({ children, heading, subHeading, body }) => {
  return (
      <InfoCardContainer>
        { children }
        <InfoCardHeading children={heading}/>
        <InfoCardSubHeading children={subHeading}/>
        <InfoCardDivider/>
        <InfoCardBody children={body}/>
      </InfoCardContainer>
  );
};

const ImageContainer = styled.div.attrs((props) => ({
    offsetLeft: props.offsetX,
    offsetTop: props.offsetY,
    scale: props.scale
}))`
    display: flex;
    flex-direction: column;
    justify-items: center;
    align-items: center;
    width: 100%;
    height: 200px;
    overflow: hidden;
    clip-path: circle(100px at center);
    margin-bottom: 3em;

    & > ${BaseImage} {
      left:   ${props => props.offsetX};
      top:    ${props => props.offsetY};
      height: ${props => props.scale};
    }
`;


const InfoCardPhoto = ({ src, offsetX, offsetY, scale }) => {
    return (
        <ImageContainer offsetX={offsetX} offsetY={offsetY} scale={scale}>
          <BaseImage src={src}/>
        </ImageContainer>
    );
};

export { InfoCardPhoto, InfoCardContainer };
export default InfoCard;
